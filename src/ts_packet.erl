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

decode_data(BinData) when(byte_size(BinData) >= ?TSLEN) ->
    <<Packet:?TSLEN/binary, Rest/binary>> = BinData,
    [decode(Packet) | decode_data(Rest)];
decode_data(_) ->
    [].

decode(BinData) when byte_size(BinData) == ?TSLEN ->
    unpack_ad(dec(BinData));
decode(BinData) -> error({unexpected, {size, byte_size(BinData)}}).

dec(?TS_b) -> ?TS_t.

get_flags({ts, _PID, FLAGS, _AD, _Payload}) ->
    FLAGS.

get_PID({ts, {pid, PID}, _FLAGS, _AD, _Payload}) ->
    PID.

get_payload({ts, _PID, _FLAGS, _AD, {payload, Payload}}) ->
    Payload.

%% Unpacks adaptation field (two step TS decoding)
unpack_ad({ts, _PID, _FLAGS, {ad, 0}, _Payload})       -> %
    error(wrong_adaptation_flag);
unpack_ad({ts, PID, FLAGS, {ad, 1}, Payload})          ->
    {ts, PID, FLAGS, {ad, none}, Payload};
unpack_ad({ts, PID, FLAGS, {ad, AD}, {payload, Data}}) ->
    <<AdLen:8/unsigned, Rest/binary>> = Data,
    AdSize = AdLen,
    <<Adaptation:(AdSize)/binary, Payload/binary>> = Rest,
    case AD of
	2 -> {ts, PID, FLAGS, {ad, Adaptation}, {payload, none}};
	3 -> {ts, PID, FLAGS, {ad, Adaptation}, {payload, Payload}};
	_    -> error(wrong_adaptation)
    end.

encode(Ts) ->
    enc(pack_ad(Ts)).

enc(?TS_t) when byte_size(Payload) == 184 -> ?TS_b;
enc({ts, _P, _F, _A, Payload}) ->
    error({unexpected, {size, byte_size(Payload)}});
enc(_)     -> error(unexpected).


pack_ad({ts, PID, FLAGS, {ad, none}, Payload})                  ->
    {ts, PID, FLAGS, {ad, 1}, Payload};
pack_ad({ts, PID, FLAGS, {ad, Adaptation}, {payload, none}})    ->
    AdLen = byte_size(Adaptation),
    Len = ?TSLEN-4-AdLen-1,
    Pad = << <<255/unsigned>> || _ <- lists:seq(1,Len) >>,
    {ts, PID, FLAGS, {ad, 2},
     {payload,
      <<AdLen:8/unsigned, Adaptation/binary, Pad/binary>>}};
pack_ad({ts, PID, FLAGS, {ad, Adaptation}, {payload, Payload}}) ->
    AdLen = byte_size(Adaptation),
    {ts, PID, FLAGS, {ad, 3},
     {payload,
      <<AdLen:8/unsigned, Adaptation/binary,Payload/binary>>}}.

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


decode_ad(<<Len:8/integer,
	    Discont:1,
	    RandAcc:1,
	    Priority:1,
	    PCRFlag:1,
	    OPCRFlag:1,
	    Splice:1,
	    TPData:1,
	    AdaptFieldExtension:1,
	    Rem/binary>>) ->
    if (Len < 2) -> throw(zeroPacketLength);
       true ->
	    L = Len-1,
	    <<Rest:L/binary, _DontCare/binary>> = Rem,
	    Flags = {Discont, RandAcc, Priority,
		     PCRFlag, OPCRFlag, Splice,
		     TPData, AdaptFieldExtension},
	    case {PCRFlag, OPCRFlag} of
		{1,1} ->
		    <<PCR:33/integer-big, _Pad:6, _Ex:9,
		      OPCR:33/integer-big, _OPad:6, _OEx:9,
		      SpliceCountdown:8/integer, _Stuffing/binary>> = Rest,
		    {adaptation, Flags,
		     {pcr, PCR}, {opcr, OPCR}, SpliceCountdown};
		{0,0} ->
		    <<SpliceCountdown:8/integer, _Stuffing/binary>> = Rest,
		    {adaptation, Flags,
		     {pcr, none}, {opcr, none}, SpliceCountdown};
		{1,0} ->
		    <<PCR:33/integer-big, _Pad:6, _Ex:9,
		      SpliceCountdown:8/integer, _Stuffing/binary>> = Rest,
		    {adaptation, Flags,
		     {pcr, PCR}, {opcr, none}, SpliceCountdown};
		{0,1} ->
		    <<OPCR:33/integer-big, _OPad:6, _OEx:9,
		      SpliceCountdown:8/integer, _Stuffing/binary>> = Rest,
		    {adaptation, Flags,
		     {pcr, none}, {opcr, OPCR}, SpliceCountdown}
	    end
    end.
