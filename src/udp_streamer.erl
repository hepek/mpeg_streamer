%%% @author Milan Markovic  <zivotinja@gmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%% udp_streamer streams mpeg-ts packets over udp bcast
%%% @end

-module(udp_streamer).
%-compile(export_all).
-export([start_link/3, stop/1, init/4]).

-include("../include/mpegts.hrl").

-type fname()  :: string().
-type stream() :: {{integer(),integer(),integer()}, integer()}.

-record(state, {src :: term(),
		dst :: term(),
		pcr_pid  :: integer(),
		pcr_last :: integer(),
		tprev,
	        ping_pid,
	        pkt_count = 0,
	        drop_interval = infinity}).

-define(CHUNKSZ, 7*?TSLEN).
-define(TIMEOUT, 5000).

start_link(ReportTo, Src, Dst) ->
    spawn_link(?MODULE, init, [ReportTo, Src, Dst]).

stop(Pid) ->
    Pid ! {self(), stop},
    Ref = monitor(process, Pid),

    receive
	stop ->
	    demonitor(Ref, [flush]),
	    {ok, normal};
	{'DOWN', Ref, process, Pid, Reason} ->
	    {ok, Reason}
    end.

init(ReportTo, Ref, SrcUri, DstUri) ->
    case (catch begin
		   S = get_src(SrcUri),
		   D = {udp, _, _, _} = get_dst(DstUri),
		   timer:send_after(10, {send, <<>>}), %% play
		   {ok, S, D}
	       end)
    of
	{ok, Src, Dst} ->
	    ReportTo ! {Ref, ok},
	    loop(#state{src=Src, dst=Dst, tprev=now(), ping_pid=ReportTo});
	Err ->
	    ReportTo ! {Ref, Err}
    end.

get_dst(DstUri) ->
    case uri_utils:parse(DstUri) of
	{udp_addr, Host, Port} ->
	    {udp, Sock} = uri_utils:open(DstUri, sender),
	    {udp, Sock, Host, Port};
	_ -> {error, cannot_parse_uri, DstUri}
    end.

get_src(SrcUri) ->
    uri_utils:open(SrcUri).

next_chunk({file, File}) ->
    file:read(File, ?CHUNKSZ);
next_chunk({udp, Sock}) ->
    case gen_udp:recv(Sock, 0, ?TIMEOUT) of
	{ok, {_Address, _Port, Packet}} ->
	    Packet;
	{error, timeout} ->
	    eof
    end.

getPCR([]) ->
    none;
getPCR([H|T]) ->
    case H#ts.ad of
	undefined -> getPCR(T);
	<<>> -> none;
	Data ->
	    Ad = ts_packet:decode_ad(Data),
	    case Ad#ad.pcr of
		undefined -> getPCR(T);
		PTS -> PTS
	    end
    end.

ping(Pid, Count, Interval) 
  when is_pid(Pid), Count rem Interval =:= 0 ->
       Pid ! Count,
       ok;
ping(_,_,_) ->
    ok.

to_wait(T) when (T >= 0) ->
    T;
to_wait(T) ->
    error_logger:warning_msg("running late ~p~n", [T]),
    0.

wait(none, Time, S) ->
    {0, S#state{tprev = Time}};
wait(PCR, Time, S = #state{tprev = _Tprev, pcr_last = undefined}) ->
    {0, S#state{tprev = Time, pcr_last = PCR}};
wait(PCR, Time, S = #state{tprev = Tprev, pcr_last = PCRlast}) ->
    Tdiff = timer:now_diff(Time, Tprev),
    ToWait = to_wait((PCR-PCRlast) div 90) - (Tdiff div 1000),
    {ToWait, S#state{tprev = Time, pcr_last = PCR}}.


send(Sock, Host, Port, Out) ->
    send(Sock, Host, Port, Out, 30).

send(_, _, _, _, 0) ->
    error(socket_closed);
send(Sock, Host, Port, Out, Retries) ->
    case gen_udp:send(Sock, Host, Port, Out) of
	ok -> ok;
	{error, eagain} -> 
	    error_logger:info_msg(":"),
	    timer:sleep(100),
	    send(Sock, Host, Port, Out, Retries-1)
    end.

dispatch(S, Packets) ->    
    PCR = getPCR(Packets),
    NPackets  = S#state.pkt_count,
    {Sleep, S1} = wait(PCR, now(), S),
    Out = [ts_packet:encode(P) || P <- Packets],
    timer:send_after(Sleep, {send, Out}),
    ping(S#state.ping_pid, NPackets, 500),
    S1#state{pkt_count = NPackets + 1}.

handle_next_chunk(S, eof) ->
    error_logger:info_msg("eof~n"),
    {ok, _} = file:position(element(2,S#state.src), {bof, 0}),
    S1 = S#state{tprev = now(), pcr_last = undefined},
    handle_next_chunk(S1, next_chunk(S#state.src));
handle_next_chunk(S, {ok, Data}) ->
    Packets = ts_packet:decode_data(Data),
    dispatch(S, Packets).

loop(S) ->
    receive
	{Pid, stop} ->
	    %% todo: change to smth more general
	    file:close(element(2, S#state.src)),
	    gen_udp:close(element(2,S#state.dst)),
	    error_logger:info_msg("closing~n"),
	    Pid ! stop;
	{send, Data} ->
	    {udp, Sock, Host, Port} = S#state.dst,
	    send(Sock, Host, Port, Data),
	    S1 = handle_next_chunk(S, next_chunk(S#state.src)),
	    loop(S1)
    end.
