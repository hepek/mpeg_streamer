%%% @author Milan Markovic  <zivotinja@gmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%% ts_packet decodes mpeg-ts packets
%%% @end

-module(ts_packet).
-compile(export_all).
%-export([decode/1, encode/1]).

-include("../include/mpegts.hrl").

-spec decode_data(binary()) -> [#ts{}].
-spec decode(binary()) -> #ts{}  | junk | too_few_bytes.
-spec dec(binary()) -> tuple() | junk | too_few_bytes.
-spec enc(tuple()) -> binary().
-spec decode_ad(binary()) -> #ad{}.

%% Decodes all incoming data skipping all bad packets or junk
%% returns only what could be decoded
decode_data(<<>>)    -> [];
decode_data(BinData) when byte_size(BinData) < ?TSLEN -> [];
decode_data(BinData) ->
    <<Packet:?TSLEN/binary, Rest/binary>> = BinData,
    case decode(Packet) of
	junk		-> decode_data(resync(BinData, 0, 3*?TSLEN));
	too_few_bytes	-> [];
	TS = #ts{}	-> [TS | decode_data(Rest)]
    end.

resync(_Data, Max, Max) -> error({sync_byte_not_found, Max});
resync(<<>>, _, _)      -> <<>>;
resync(Data, Counter, Max) ->
    warning_msg("junk on input, resynchronizing stream~n"),
    <<_Skip:8, Data2/binary>> = Data,
    <<Syn:8, _/binary>> = Data2,
    case Syn of
	?SYNCB -> Data2;
	_ -> resync(Data2, Counter+1, Max)
    end.


decode(BinData) when byte_size(BinData) == ?TSLEN ->
    case dec(BinData) of
	junk   -> junk;
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
	_ -> error(wrong_adaptation)
    end.

encode(Ts) ->
    enc(pack_ad(Ts)).

enc(?TS_t) -> 
    ?TS_b;
enc(_)     -> 
    error(unexpected).


padding(Data) ->
    PLen = byte_size(Data),
    Len = ?TSLEN-4-PLen,
    iolist_to_binary([Data, << <<255>> || _ <- lists:seq(1,Len) >>]).

pack_ad(P = #ts{ad=undefined})  ->
    D = padding(P#ts.payload),
    {ts_t, P#ts.pid, P#ts.flags, 1, D};
pack_ad(P = #ts{payload=undefined}) ->
    Adaptation = P#ts.ad,
    AdLen = byte_size(Adaptation),
    D = padding(<<AdLen:8/unsigned, Adaptation/binary>>),
    {ts_t, P#ts.pid, P#ts.flags, 2, D};
pack_ad(P = #ts{}) ->    
    Adaptation = P#ts.ad,
    AdLen = byte_size(Adaptation),
    Payload = P#ts.payload,
    D = padding(<<AdLen:8, Adaptation/binary, Payload/binary>>),
    {ts_t, P#ts.pid, P#ts.flags, 3, D}.

decode_PAT(<<_PF:8, 0:8, 1:1, 0:1, 2#11:2, 0:2,
	     SectionLength:10,
	     _TransportStreamId:16,
	     2#11:2,
	     VersionNo:5,
	     CurrNext:1,
	     SectionNo:8, LastSectionNo:8,
	     Rest/binary>>) ->
    ProgLen = SectionLength-9,
    <<Data:ProgLen/binary, _CRC:32, _/binary>> = Rest,
    Programs = [{ProgramNum, PID} || <<ProgramNum:16, _Res:3, PID:13>> <= Data],
    {pat, {VersionNo, CurrNext, SectionNo, LastSectionNo}, Programs}.

decode_PMT(<<0:8, 2:8, SS:1, 0:1, XX:2, 0:2, Len:10, Rest:Len/binary, _/binary>>) ->
    case crc:crc(<<2:8, SS:1, 0:1, XX:2, 0:2, Len:10, Rest:Len/binary>>) of
	0 -> SL = Len-4,
	     <<Strip:SL/binary, _CRC32:32/big-integer>> = Rest,
	     <<ProgramNum:16, _:2, _Ver:5, _CN:1, 0:8, 0:8, _:3, PCRPID:13, _:4, 0:2, PIL:10, _PI:PIL/binary, Progs/binary>> = Strip,
	     {pmt, ProgramNum, PCRPID,
	      [{Stype, EPID, ESL, Desc} ||
		  <<Stype:8, _:3, EPID:13, _:4, 0:2, ESL:10, Desc:ESL/binary>> <= Progs]};
	N -> {error, bad_checksum, N}
    end.
%%
%%decode_PMT(<<0:8, 2:8, SS:1, 0:1, XX:2, 0:2, Len:10,
%%	     Rest:Len/binary, _/binary>>) ->
%%    %%0 = crc:crc(<<0:8, 2:8, SS:1, 0:1, XX:2, 0:2, Len:10, Rest:Len/binary>>),
%%    SL = Len-4,
%%    <<Strip:SL/binary, _CRC32:32/big-integer>> = Rest,
%%    <<ProgramNum:16, _:2, _Ver:5, _CN:1, 0:8, 0:8, _:3, PCRPID:13, _:4, 0:2, PIL:10, _PI:PIL/binary, Progs/binary>> = Strip,
%%    {pmt, ProgramNum, PCRPID,
%%     [{Stype, EPID, ESL, Desc} ||
%%	 <<Stype:8, _:3, EPID:13, _:4, 0:2, ESL:10, Desc:ESL/binary>> <= Progs]}.


append_PMT(<<0:8, 2:8, SS:1, 0:1, 2#11:2, 0:2, Len:10,
	    Rest:Len/binary, RestRest/binary>>, {AStype, AEPID, AESL, ADESC}) ->
    SL = Len-4,
    <<Strip:SL/binary, _CRC32:32/big-integer>> = Rest,
    <<ProgramNum:16,
      _:2, Ver:5,
      CN:1, 0:8, 0:8, _:3,
      PCRPID:13, _:4, 0:2, PIL:10, PI:PIL/binary,
      Progs/binary>> = Strip,
	  Appendix = <<AStype:8, 2#11:3, AEPID:13, 0:4, 0:2, AESL:10, ADESC:AESL/binary>>,
	  Len2 = Len + byte_size(Appendix),
	  OutH = <<2:8, SS:1, 0:1, 2#11:2, 0:2, Len2:10>>,
	  OutH2 = <<ProgramNum:16, 0:2, Ver:5, CN:1, 0:8, 0:8, 0:3,
				PCRPID:13, 0:4, 0:2, PIL:10, PI:PIL/binary>>,
	  OutB = <<Progs/binary, Appendix/binary>>,
	  Fill = binary:part(RestRest, {byte_size(Appendix), byte_size(RestRest)-byte_size(Appendix)}),
	  Data = <<OutH/binary, OutH2/binary, OutB/binary>>,
	  CRC32New = crc:crc(Data),
	  <<0:8, Data/binary, CRC32New/binary, Fill/binary>>.

get_PCR(Data, 1) ->
    <<PCR:33/integer-big, _Pad:6, _Ex:9, Rest/binary>> = Data,	    
    {PCR, Rest};
get_PCR(Data, 0) ->
    {undefined, Data}.

get_Splice(_Data, 0) ->
    undefined;
get_Splice(Data, 1) ->
    <<SpliceC0:8, _/binary>> = Data,
    SpliceC0.

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
    
    {PCR, Rest1}    = get_PCR(Rest,  PCRFlag),
    {OPCR, Rest2}   = get_PCR(Rest1, OPCRFlag),
    SpliceCountdown = get_Splice(Rest2, Splice),
    
    #ad{flags = Flags, pcr = PCR, opcr = OPCR, 
	splice_countdown = SpliceCountdown}.
