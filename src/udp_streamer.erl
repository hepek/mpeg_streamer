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
	        pkt_count = 0}).

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

sleep2(T) when (T >= 0) ->
    timer:sleep(T);
sleep2(T) ->
    io:format("running late ~p~n", [T]),
    ok.

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


wait(none, Time, S) ->
    S#state{tprev = Time};
wait(PCR, Time, S = #state{tprev = _Tprev, pcr_last = undefined}) ->
    S#state{tprev = Time, pcr_last = PCR};
wait(PCR, Time, S = #state{tprev = Tprev, pcr_last = PCRlast}) ->
    Tdiff = timer:now_diff(Time, Tprev),
    sleep2(((PCR-PCRlast) div 90) - (Tdiff div 1000)),
    S#state{tprev = Time, pcr_last = PCR}.

dispatch(S, Packets) ->
    {udp, Sock, Host, Port} = S#state.dst,
    PCR = getPCR(Packets),
    S1  = wait(PCR, now(), S),
    Out = [ts_packet:encode(P) || P <- Packets],
    ok  = gen_udp:send(Sock, Host, Port, Out),
    ping(S#state.ping_pid, S#state.pkt_count, 500),
    S1#state{pkt_count = S#state.pkt_count + 1}.

loop(S) ->
    receive
	{Pid, stop} ->
	    %% todo: change to smth more general
	    file:close(element(2, S#state.src)),
	    gen_udp:close(element(2,S#state.dst)),
	    io:format("closing~n"),
	    Pid ! stop
    after
	0 -> case next_chunk(S#state.src) of
		 eof ->
		     io:format("eof~n"),
		     {ok, _} = file:position(element(2,S#state.src), {bof, 0}),
		     loop(S);
		     %exit(normal);
		 {ok, Data} ->
		     Packets = ts_packet:decode_data(Data),
		     S1 = dispatch(S, Packets),
		     loop(S1)
	     end
    end.
