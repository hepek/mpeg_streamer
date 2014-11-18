-module(ms_udp).

%% API
%-export([scan/2]).
-compile(export_all).

-record(state, {inProgress = gb_sets:new(),
		result = []}).

scanSeq(IPSpec, PortSpec) ->
    IPS = expandIPs(IPSpec),
    Ports = expandPorts(PortSpec),
    error_logger:info_msg("Scanning ~p addresses~n", [length(IPS)*length(Ports)]),
    Res = [report(IP, PORT) || IP <- IPS, PORT <- Ports],
    lists:filter(fun (X) -> not (X =:= no_stream) end, Res).

scanParallel(IPSpec, PortSpec) ->
    IPS = expandIPs(IPSpec),
    Ports = expandPorts(PortSpec),
    Confs = [{IP, PORT} || IP <- IPS, PORT <- Ports],
    error_logger:info_msg("Scanning ~p addresses~n", [length(Confs)]),
    loop(#state{inProgress = init(Confs)}).

init(X) ->
    init(X, gb_sets:new()).

init([], Set) ->
    Set;
init([{IP, PORT} | XS], Set) ->
    Ref = make_ref(),
    Pid = self(),
    spawn_link(fun () -> report2(Pid, Ref, IP, PORT) end),
    init(XS, gb_sets:add_element(Ref, Set)).

loop(State = #state{result = Result, inProgress = Set}) ->
    case gb_sets:is_empty(Set) of
	true ->
	    Result;
	false ->
	    receive
		{Ref, no_stream} ->
		    loop(State#state{inProgress = gb_sets:del_element(Ref, Set)});
		{Ref, Reply} ->
		    loop(State#state{result = [Reply | Result], inProgress = gb_sets:del_element(Ref, Set)});
		Msg -> 
		    io:format("Unexpected: ~p~n", [Msg]),
		    loop(State)
	    end
    end.

report2(Pid, Ref, IP, Port) ->
    Res = report(IP, Port),
    Pid ! {Ref, Res}.

report(IP, PORT) ->
    URI = gen_uri(IP, PORT),
    case stream_info:udp_str_info(URI, 512) of
	{Bitrate, Progs} -> {URI, Bitrate, Progs};
	no_stream -> no_stream
    end.

gen_uri({A, B, C, D}, Port) ->
    lists:flatten(io_lib:format("udp://~p.~p.~p.~p:~p", [A, B, C, D, Port])).

expandIPs({{A0,A1}, {B0, B1}, {C0, C1}, {D0, D1}}) ->
    [{A, B, C, D} || A <- lists:seq(A0, A1)
                   , B <- lists:seq(B0, B1)
                   , C <- lists:seq(C0, C1)
                   , D <- lists:seq(D0, D1)];
expandIPs({A, {B0, B1}, {C0, C1}, {D0, D1}}) ->
    expandIPs({{A, A}, {B0, B1}, {C0, C1}, {D0, D1}});
expandIPs({A, B, {C0, C1}, {D0, D1}}) ->
    expandIPs({{A, A}, {B, B}, {C0, C1}, {D0, D1}});
expandIPs({A, B, C, {D0, D1}}) ->
    expandIPs({{A, A}, {B, B}, {C, C}, {D0, D1}});
expandIPs({A, B, C, D}) ->
    [{A, B, C, D}].

expandPorts({A, B}) ->
    lists:seq(A, B);
expandPorts(A) ->
    [A].
