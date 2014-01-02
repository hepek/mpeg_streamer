%%% @author Milan Markovic  <zivotinja@gmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%% ts_packet decodes mpeg-ts packets
%%% @end
%%% Created : 27 Dec 2013 by  <milan@epikur>
%%% Note:

-module(ts_packet).
-compile(export_all).
%-export([decode/1, encode/1]).

-include("../include/mpegts.hrl").

-spec decode_data(binary()) -> [#ts{}].
-spec decode(binary()) -> #ts{}  | junk | too_few_bytes.
-spec dec(binary()) -> tuple() | junk | too_few_bytes.
-spec enc(tuple()) -> binary().

%% Decodes all incoming data skipping all bad packets or junk
%% returns only what could be decoded
decode_data(<<>>)    -> [];
decode_data(BinData) ->
    <<Packet:?TSLEN/binary, Rest/binary>> = BinData,
    case decode(Packet) of
	junk		-> decode_data(resync(BinData, 0, 100*?TSLEN));
	too_few_bytes	-> [];
	TS = #ts{}	-> [TS | decode_data(Rest)]
    end.

resync(_Data, Max, Max) -> error({sync_byte_not_found, Max});
resync(<<>>, _, _)      -> <<>>;
resync(Data, Counter, Max) ->
    io:format("junk on input, resynchronizing stream~n"),
    <<_Skip:8, Data2/binary>> = Data,
    <<Syn:8, _/binary>> = Data2,
    case Syn of
	?SYNCB -> Data2;
	_ -> resync(Data2, Counter+1, Max)
    end.


decode(BinData) when byte_size(BinData) == ?TSLEN ->
    case dec(BinData) of
	junk -> junk;
	Result -> unpack_ad(Result)
    end;
decode(_BinData) -> 
    too_few_bytes.

dec(?TS_b) -> ?TS_t;
dec(_) -> junk.

%% Unpacks adaptation field (two step TS decoding)
unpack_ad({ts_t, _PID, _FLAGS, 0, _Payload})       -> %
    error(wrong_adaptation_flag);
unpack_ad({ts_t, PID, FLAGS, 1, Payload})          ->
    #ts{pid=PID, flags=FLAGS, payload=Payload};
unpack_ad({ts_t, PID, FLAGS, AD, Data}) ->    
    <<AdLen:8/unsigned, Rest/binary>> = Data,
    AdSize = AdLen,
    <<Adaptation:(AdSize)/binary, Payload/binary>> = Rest,
    case AD of
	2 -> #ts{pid=PID, flags=FLAGS, ad=Adaptation};
	3 -> #ts{pid=PID, flags=FLAGS, ad=Adaptation, payload=Payload};
	_    -> error(wrong_adaptation)
    end.

encode(Ts) ->
    enc(pack_ad(Ts)).

enc(?TS_t) when byte_size(Payload) == 184 -> ?TS_b;
enc({ts_t, _P, _F, _A, Payload}) ->
    error({unexpected, {size, byte_size(Payload)}});
enc(_)     -> error(unexpected).


pack_ad(P = #ts{ad=undefined})  ->
    {ts_t, P#ts.pid, P#ts.flags, {ad, 1}, P#ts.payload};
pack_ad(P = #ts{payload=undefined}) ->
    Adaptation = P#ts.ad,
    AdLen = byte_size(Adaptation),
    Len = ?TSLEN-4-AdLen-1,
    Pad = << <<255/unsigned>> || _ <- lists:seq(1,Len) >>,
    {ts_t, P#ts.pid, P#ts.flags, 2,
      <<AdLen:8/unsigned, Adaptation/binary, Pad/binary>>};
pack_ad(P = #ts{}) ->    
    Adaptation = P#ts.ad,
    AdLen = byte_size(Adaptation),
    Payload = P#ts.payload,
    {ts_t, P#ts.pid, P#ts.flags, 3,
      <<AdLen:8/unsigned, Adaptation/binary, Payload/binary>>}.

decode_PAT(<<_PF:8, 0:8, 1:1, 0:1, 2#11:2, 0:2
	    , SectionLength:10
	    , _TransportStreamId:16
	    , 2#11:2
	    , VersionNo:5
	    , CurrNext:1
	    , SectionNo:8, LastSectionNo:8
	    , Rest/binary>>) ->
    ProgLen = SectionLength-9,
    <<Data:ProgLen/binary,
      _CRC:32, %TODO: check CRC
      _/binary>> = Rest,
    Programs = [{ProgramNum, PID} || <<ProgramNum:16, _Res:3, PID:13>> <= Data],
    {pat, {VersionNo, CurrNext, SectionNo, LastSectionNo}, Programs}.

decode_PMT(<<0:8, 2:8, _SS:1, 0:1, _:2, 0:2, Len:10,
	    Rest:Len/binary, _/binary>>) ->
    SL = Len-4,
    <<Strip:SL/binary, _CRC32:32/big-integer>> = Rest,
    <<ProgramNum:16
      , _:2, _Ver:5
      , _CN:1, 0:8, 0:8, _:3
      , PCRPID:13, _:4, 0:2, PIL:10, _PI:PIL/binary
      , Progs/binary>> = Strip,
    {pmt, ProgramNum, PCRPID,
     [{Stype, EPID, ESL, Desc} ||
	 <<Stype:8, _:3, EPID:13, _:4, 0:2, ESL:10, Desc:ESL/binary>> <= Progs]}.

get_PCR(Data) ->
    <<PCR:33/integer-big, _Pad:6, _Ex:9, Rest/binary>> = Data,
    {PCR, Rest}.

decode_ad(<<Discont:1,
	    RandAcc:1,
	    Priority:1,
	    PCRFlag:1,
	    OPCRFlag:1,
	    Splice:1,
	    TPData:1,
	    AdaptFieldExtension:1,
	    Rest/binary>>) ->    
    Flags = {Discont, RandAcc, Priority,
	     PCRFlag, OPCRFlag, Splice,
	     TPData, AdaptFieldExtension},
    {PCR, OPCR, AfterPCR} =
	case {PCRFlag, OPCRFlag} of
	    {1,1} ->
		{PCR0, Rest2} = get_PCR(Rest),
		{OPCR0, Rest3} = get_PCR(Rest2),
		{PCR0, OPCR0, Rest3};
	    {0,0} ->
		{undefined, undefined, Rest};
	    {1,0} ->
		{PCR0, Rest2} = get_PCR(Rest),
		{PCR0, undefined, Rest2};
	    {0,1} ->
		{OPCR0, Rest2} = get_PCR(Rest),
		{undefined, OPCR0, Rest2}
	end,

    SpliceCountdown =
	case Splice of
	    1 -> <<SpliceC0:8, _/binary>> = AfterPCR,
		 SpliceC0;
	    _ -> undefined
	end,
	
    #ad{flags = Flags, pcr = PCR, opcr = OPCR, 
	splice_countdown = SpliceCountdown}.



%% Note: does not work 
lshift(<<B:1, Rest/bitstring>>) ->
    {B, <<Rest/bitstring, 0:1>>}.

crc32h(<<>>, CrcReg) -> 
    CrcReg;
crc32h(<<D:1, Data/bitstring>>, CrcReg) ->
    {Out, <<New:32>>} = lshift(<<CrcReg:32>>),
    io:format("~p, ~p~n", [Out, New]),
    if (D =/= Out) ->
	    crc32h(Data, New);
       true ->
	    crc32h(Data, New bxor 16#04c11db7)
    end.

crc32(Data) ->
    crc32h(Data, 16#FFFFFFFF) bxor 16#FFFFFFFF.
    

%% DD1 = <<0,0,176,13,0,1,193,0,0,0,1,224,80,66,119,52,166>>.
%% P = <<16#04c11db7:32>>.
