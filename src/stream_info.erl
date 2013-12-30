%%% @author Milan Markovic  <zivotinja@gmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%% This module prints ts stream info
%%% @end
%%% Created : 27 Dec 2013 by  <milan@epikur>

-module(stream_info).
-compile(export_all).
-author('zivotinja@gmail.com').
-import(ts_packet,
	[decode_PAT/1, decode_PMT/1, decode_data/1]).

-include("../include/mpegts.hrl").

filter(PID, Packets) ->
    lists:filter(fun (Packet) -> Packet#ts.pid =:= PID end, Packets).

file_info(FileName) ->
    file_info(FileName, 1024).

file_info(FileName, NumPackets) ->
    {ok, File} = file:open(FileName,  [binary, read]),
    {ok, Data} = file:read(File, NumPackets*?TSLEN),
    file:close(File),
    Packets = decode_data(Data),
    info(Packets).

udp_info(Bcast, Port) ->
    udp_info(Bcast, Port, 1024).

udp_info(Bcast, Port, NumPackets) ->
    {ok, S} = gen_udp:open(Port, [{active, false}, binary]),
    case Bcast of
	{A, B, C, D} -> 
	    inet:setopts(S, [{add_membership, {{A,B,C,D}, {0,0,0,0}}}])
    end,
    Data = receive_udp(S, NumPackets*?TSLEN),
    %inet:setopts(S, [{drop_membership, {{A,B,C,D}, {0,0,0,0}}}])
    gen_udp:close(S),
    Packets = lists:flatmap(fun ts_packet:decode_data/1, Data),
    info(Packets).

receive_udp(_, More) when (More =< 0) ->
    [];
receive_udp(Sock, More) ->
    {ok, {_, _, Data}} = gen_udp:recv(Sock, 0, 2000),
    [Data | receive_udp(Sock, More-byte_size(Data))].

prog_pcr(Info) ->
    prog_pcr(Info, 1).
prog_pcr(Info, Program) ->
    {program, _, PCR, _} = lists:nth(Program, Info),
    PCR.

info(Packets) ->
    P = filter(0, Packets),
    TS1 = hd(P),
    {pat, _, Progs} = decode_PAT(TS1#ts.payload),
    [prog_info(Prog, Packets) || Prog <- Progs].

prog_info({ProgNum, PID}, Packets) ->
    P = filter(PID, Packets),
    TS1 = hd(P),
    {pmt, _, PCRPID, Streams} = decode_PMT(TS1#ts.payload),
    {program, ProgNum, PCRPID, lists:map(fun stream_info/1, Streams)}.

stream_info({Stype, PID, _ESL, Desc}) ->
    {PID, stype2str(Stype), ex_desc(Desc)}.


ex_desc(<<>>) ->
    <<"">>;
ex_desc(<<T, L, Desc:L/binary>>) ->
    {desc_type(T), Desc}.

desc_type(1)  -> reserved;
desc_type(2)  -> video_stream_descriptor;
desc_type(3)  -> audio_stream_descriptor;
desc_type(4)  -> hierarchy_descriptor;
desc_type(5)  -> registration_descriptor;
desc_type(6)  -> data_stream_alignment_descriptor;
desc_type(7)  -> target_background_grid_descriptor;
desc_type(8)  -> video_window_descriptor;
desc_type(9)  -> 'CA_descriptor';
desc_type(10) -> 'ISO_639_language_descriptor';
desc_type(11) -> system_clock_descriptor;
desc_type(12) -> multiplex_buffer_utilization_descriptor;
desc_type(13) -> copyright_descriptor;
desc_type(14) -> maximum_bitrate_descriptor;
desc_type(15) -> private_data_indicator_descriptor;
desc_type(16) -> smoothing_buffer_descriptor;
desc_type(17) -> 'STD_descriptor';
desc_type(18) -> 'IBP_descriptor';
desc_type(27) -> 'MPEG-4_video_descriptor';
desc_type(28) -> 'MPEG-4_audio_descriptor';
desc_type(29) -> 'IOD_descriptor';
desc_type(30) -> 'SL_descriptor';
desc_type(31) -> 'FMC_descriptor';
desc_type(32) -> 'External_ES_ID_descriptor';
desc_type(33) -> 'MuxCode_descriptor';
desc_type(34) -> 'FmxBufferSize_descriptor';
desc_type(35) -> 'MultiplexBuffer_descriptor';
desc_type(36) -> 'FlexMuxTiming_descriptor';
desc_type(D) when((D =< 19) and (D >= 63)) -> 'reserved';
desc_type(_) -> 'user_private'.


stype2str(1)   -> mpeg1_video;				%"ISO/IEC 11172 Video (MPEG-1)";
stype2str(2)   -> mpeg2_video;				%"ITU-T Rec. H.262 | ISO/IEC 13818-2 (MPEG-2)
						        %Video or ISO/IEC 11172-2 (MPEG-1) constrained parameter video stream";
stype2str(3)   -> mpeg1_audio;				%"ISO/IEC 11172 Audio (MPEG-1) NOT USED by ATSC";
stype2str(4)   -> mpeg2_audio;				%"ISO/IEC 13818-3 Audio (MPEG-2) NOT USED by ATSC";
stype2str(5)   -> mpeg2_private_table_sections;		%"MPEG-2 private table sections";
stype2str(6)   -> mpeg2_pes_private_data;		%"MPEG-2 Packetized Elementary Stream packets containing private data";
stype2str(7)   -> mheg_packets;				%"MHEG Packets";
stype2str(8)   -> mpeg2_annexA_DSM_CC;			%"MPEG-2 Annex A DSM CC";
stype2str(9)   -> 'ITU-T Rec. H.222.1';
stype2str(10)  -> 'ISO/IEC 13818-6 type A';
stype2str(11)  -> 'ISO/IEC 13818-6 type B';
stype2str(12)  -> 'ISO/IEC 13818-6 type C';
stype2str(13)  -> 'ISO/IEC 13818-6 type D';
stype2str(14)  -> 'ISO/IEC 13818-1 (MPEG-2) auxiliary';
stype2str(15)  -> 'ISO/IEC 13818-7 Audio with ADTS transport syntax';
stype2str(16)  -> mpeg4_video;				%"ISO/IEC 14496-2 (MPEG-4) Visual;
stype2str(17)  -> 'ISO/IEC 14496-3 Audio with the LATM transport syntax as defined in ISO/IEC 14496-3 / AMD 1';
stype2str(18)  -> 'ISO/IEC 14496-1 SL-packetized stream or FlexMux stream carried in PES packets';
stype2str(19)  -> 'ISO/IEC 14496-1 SL-packetized stream or FlexMux stream carried in ISO/IEC14496_sections';
stype2str(20)  -> 'ISO/IEC 13818-6 Synchronized Download Protocol';
stype2str(21)  -> metadata_in_pes;			%"Metadata carried in PES packets";
stype2str(22)  -> metadata_in_metadata_sections;	%"Metadata carried in metadata_sections";
stype2str(129) -> atsc_AC3_audio;			%"ATSC AC-3 Audio";
stype2str(_)   -> mpeg2_user_private.			%"MPEG-2 User Private".
