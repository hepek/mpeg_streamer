-module(muxfiles).
-compile(export_all).

toPackets(FileName) ->
	{ok, Data} = file:read_file(FileName),
	ts_packet:decode_data(Data).

interlieve(ListA, ListB) ->
	AL = length(ListA),
	BL = length(ListB),
	ChunkSZ = AL div BL,
	AChunked = [lists:sublist(ListA, X, ChunkSZ) || X <- lists:seq(1, AL, ChunkSZ)],
	lists:zipwith(fun (A,B) -> [A,B] end, lists:sublist(AChunked, 1, BL), ListB).

append_all_PMT([], _) ->
	[];
append_all_PMT([{ts, 80, FLAGS, Ad, Payload} | Rest], Appendix) ->
	[{ts, 80, FLAGS, Ad, ts_packet:append_PMT(Payload, Appendix)} | append_all_PMT(Rest, Appendix)];
append_all_PMT([TS|Rest], Appendix) ->
	[TS | append_all_PMT(Rest, Appendix)].

go() ->
	SilverP = toPackets("D:/Misc/Silver4339B564_21FEB03__0019381__SilverZZ000999.mpg"),
	MISBP   = toPackets("D:/Misc/MISB/HD_MP2_06011_TS_ASYN_V1_001.mpg"),
	{ts, _, _, _, PMTP} = lists:nth(3,MISBP),
	{pmt, _, _, PMTL}   = ts_packet:decode_PMT(PMTP),
	MISB = [M || M = {ts, 497, _, _, _} <- MISBP],
	Muxed = lists:flatten(interlieve(SilverP, MISB)),
	MuxedWPMT = append_all_PMT(Muxed, lists:nth(2, PMTL)),
	file:write_file("D:/Misc/MuxedMetadata6_bintest.mpg",lists:map(fun ts_packet:encode/1, MuxedWPMT)).
