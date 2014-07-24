%%% @author Milan Markovic  <zivotinja@gmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%% udp_streamer streams mpeg-ts packets over udp bcast
%%% @end
%%% Created : 02 Jan 2014 by  <zivotinja@gmail.com>
%%% Note:

-module(udp_streamer).
%-compile(export_all).
-export([start_link/3, stop/1, init/3]).

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

init(ReportTo, SrcUri, DstUri) ->
    Src = get_src(SrcUri),
    Dst = get_dst(DstUri),
    loop(#state{src=Src, dst=Dst, tprev=now(), ping_pid=ReportTo}).

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
    io:format("~p,", [T]),
    T; %timer:sleep(T);
to_wait(T) ->
    io:format("running late ~p~n", [T]),
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
    case gen_udp:send(Sock, Host, Port, Out) of
	ok -> ok;
	{error, eagain} -> 
	    io:format(":"),
	    timer:sleep(100),
	    send(Sock, Host, Port, Out)
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
    io:format("eof~n"),
    {ok, _} = file:position(element(2,S#state.src), {bof, 0}),
    handle_next_chunk(S, next_chunk(S#state.src));
handle_next_chunk(S, {ok, Data}) ->
    Packets = ts_packet:decode_data(Data),
    dispatch(S, Packets).

loop(S) ->
    receive
	{Pid, stop} ->
	    %% todo: change to smth more general
	    file:close(element(2, S#state.src)),
	    gen_udp:close(element(2,S#state.dst)),
	    io:format("closing~n"),
	    Pid ! stop;
	{send, Data} ->
	    {udp, Sock, Host, Port} = S#state.dst,
	    send(Sock, Host, Port, Data),
	    S1 = handle_next_chunk(S, next_chunk(S#state.src)),
	    loop(S1)
    end.
