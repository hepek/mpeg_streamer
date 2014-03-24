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

info(URI) ->
    info(URI, 1024).

info(URI, NumPackets) ->
    case uri_utils:parse(URI) of
	{udp_addr, _, _} ->
	    udp_info(URI, NumPackets);
	{file_name, _} ->
	    file_info(URI, NumPackets);
	A -> A
    end.
	   

filter(PID, Packets) ->
    lists:filter(fun (P) -> P#ts.pid =:= PID end, Packets).

file_info(URI) ->
    file_info(URI, 1024).

file_info(URI, NumPackets) ->
    {file, File} = uri_utils:open(URI),
    {ok, Data} = file:read(File, NumPackets*?TSLEN),
    file:close(File),
    Packets = decode_data(Data),
    packets_info(Packets).

udp_info(URI) ->
    udp_info(URI, 1024).

udp_info(URI, NumPackets) ->
    {udp, S} = uri_utils:open(URI),
    D = receive_udp(S, NumPackets*?TSLEN),
    gen_udp:close(S),
    {ok, Data} = D,
    Packets = lists:flatmap(fun ts_packet:decode_data/1, Data),
    packets_info(Packets).

receive_udp(_, More) when (More =< 0) ->
    {ok, []};
receive_udp(Sock, More) ->
    case gen_udp:recv(Sock, 0, 2000) of	
	{ok, {_, _, Data}} ->
	    {ok, [Data | receive_udp(Sock, More-byte_size(Data))]};
	{error, Reason} ->
	    {error, Reason}
    end.

prog_pcr(Info) ->
    prog_pcr(Info, 1).
prog_pcr(Info, Program) ->
    {program, _, PCR, _} = lists:nth(Program, Info),
    PCR.

packets_info(Packets) ->
    case filter(0, Packets) of
	[] -> {error, no_PID_0_found};
	P -> 
	    TS1 = hd(P),
	    {pat, _, Progs} = decode_PAT(TS1#ts.payload),
	    [prog_info(Prog, Packets) || Prog <- Progs]
    end.

prog_info({ProgNum, PID}, Packets) ->
    StreamInfo = 
	fun ({2, PID2, _ESL, _Desc}) ->
		P = hd(filter(PID2, Packets)),
		{PID2, mpeg2info(P#ts.payload)};
	    ({27, PID2, _ESL, _Desc}) ->
		P = hd(filter(PID2, Packets)),
		{PID2, h264info(P#ts.payload)};
	    ({Stype, PID2, _ESL, Desc}) ->
		{PID2, stype2str(Stype), ex_desc(Desc)} end,
    P = filter(PID, Packets),
    TS1 = hd(P),
    {pmt, _, PCRPID, Streams}  = decode_PMT(TS1#ts.payload),
    {program, ProgNum, PCRPID, lists:map(StreamInfo, Streams)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mpeg2info
mpeg2info(Data) ->
    case binary:match(Data, <<0,0,1,16#b3>>) of
	{Fro, _} ->
	   <<W:12/big-integer, H:12/big-integer, AS:4, FR:4, BR:18/big-integer, 1:1, _/bitstring>> = 
		binary:part(Data, {Fro+4, 11}),
	    %{W, H, aspect(A), framerate(FP), BR*400/1024/1024};
	    lists:flatten(io_lib:format("mpeg2_video ~px~p ~s ~p Mbps ~p fps", 
					[W, H, aspect(AS), BR*400/1000/1000, framerate(FR)]));
	nomatch ->
	    mpeg2_video
    end.

aspect(1) -> "1:1";
aspect(2) -> "4:3";
aspect(3) -> "16:9";
aspect(4) -> "2.21:1";
aspect(_) -> "unknown".

framerate(1) -> 23.976;
framerate(2) -> 24;
framerate(3) -> 25;
framerate(4) -> 29.97;
framerate(5) -> 30;
framerate(6) -> 50;
framerate(7) -> 59.94;
framerate(8) -> 60;
framerate(_) -> 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% h264 info

h264info(Data) ->    
    case binary:match(Data, <<0,0,1,16#67>>) of
	{Fro, _} ->
	    SPSData = binary:part(Data, {Fro+4, 120}),
	    parseSPS(SPSData);
	nomatch ->
	    h264_video
    end.

eg1(<<0:12, N:13/big-integer, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:11, N:12/big-integer, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:10, N:11/big-integer, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:9,  N:10/big-integer, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:8,  N:9/big-integer,  Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:7,  N:8, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:6,  N:7, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:5,  N:6, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:4,  N:5, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:3,  N:4, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:2,  N:3, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<0:1,  N:2, Rest/bitstring>>) -> {N-1, Rest};
eg1(<<1:1, Rest/bitstring>>) ->       {0, Rest}.

egN(<<>>, _) -> 
    [<<>>];
egN(Data, 0) ->
    [Data];
egN(Data, N) ->
    {D, Rest} = eg1(Data),
    [D | egN(Rest, N-1)].


-record(sps, {profile_idc                  = 0,
	      constraint_set0_flag         = 0,
	      constraint_set1_flag         = 0,
	      constraint_set2_flag         = 0,
	      constraint_set3_flag         = 0,
	      constraint_set4_flag         = 0,
	      level_idc			   = 0,
	      seq_param_set_id		   = 0,
	      %% STUFF
	      log2_max_frame_num_minus4	   = 0,
	      pic_order_cnt_type	   = 0,
	      log2_max_pic_order_cnt_lsb_minus4 = 0,
	      max_num_ref_frames	   = 0,
	      gaps_in_frame_num_value_allowed_flag = 0,
	      pic_width_in_mbs_minus1	   = 0,
	      pic_height_in_map_minus1	   = 0,
	      frame_mbs_only_flag	   = 0,
	      mb_adaptive_frame_field_flag = 0,
	      direct_8x8_inference_flag	   = 0,
	      frame_cropping_flag	   = 0,
	      frame_crop_offsets	   = [],
	      vui_parameters_present_flag  = 0,
	      vui_parameters		   = []}).

parseSPS(<<PROFILE_IDC:8, _CS0:1, _CS1:1, _CS2:1, _CS3:1, _CS4:1, 0:3, _LEVEL_IDC:8, _Rest/bitstring>>)
  when PROFILE_IDC =:= 100; PROFILE_IDC =:= 110; PROFILE_IDC =:= 122; PROFILE_IDC =:= 244;
       PROFILE_IDC =:= 44;  PROFILE_IDC =:= 83;	 PROFILE_IDC =:= 86;  PROFILE_IDC =:= 118 ->
    error_cannot_parse;
parseSPS(<<PROFILE_IDC:8, CS0:1, CS1:1, CS2:1, CS3:1, CS4:1, 0:3, LEVEL_IDC:8, Rest/bitstring>>) ->
    [SEQ_PARAMETER_SET_ID,
     Rest2] = egN(Rest, 1),
    SPS0 = #sps{profile_idc          = PROFILE_IDC, 
		constraint_set0_flag = CS0,
		constraint_set1_flag = CS1,
		constraint_set2_flag = CS2,
		constraint_set3_flag = CS3,
		constraint_set4_flag = CS4,
		level_idc            = LEVEL_IDC, 
		seq_param_set_id     = SEQ_PARAMETER_SET_ID},
    sps1(SPS0, Rest2).	      

sps1(SPS0, <<Data/bitstring>>) ->
    [LOG2_MAX_FRAME_NUM_MINUS4,
     PIC_ORDER_CNT_TYPE,
     LOG2_MAX_PIC_ORDER_CNT_LSB_MINUS4,
     MAX_NUM_REF_FRAMES, Rest] = egN(Data, 4),
    <<Gaps:1, Rest2/bitstring>> = Rest,
    io:format("~w~n",[Rest2]),
    [PIC_WIDTH_IN_MBS_MINUS_1,
     PIC_HEIGHT_IN_MAP_MINUS_1,
     Rest3] = egN(Rest2, 2),
    SPS1 = SPS0#sps{
	     log2_max_frame_num_minus4     = LOG2_MAX_FRAME_NUM_MINUS4,
	     pic_order_cnt_type            = PIC_ORDER_CNT_TYPE,
	     log2_max_pic_order_cnt_lsb_minus4 = LOG2_MAX_PIC_ORDER_CNT_LSB_MINUS4,
	     max_num_ref_frames		   = MAX_NUM_REF_FRAMES,	     
	     gaps_in_frame_num_value_allowed_flag = Gaps,
	     pic_width_in_mbs_minus1	   = PIC_WIDTH_IN_MBS_MINUS_1,
	     pic_height_in_map_minus1      = PIC_HEIGHT_IN_MAP_MINUS_1},
    case Rest3 of
	<<0:1, MB_ADAPTIVE_FRAME_FIELD_FLAG:1, DIRECT_8x8_INFERENCE_FLAG:1, 1:1, Rest4/bitstring>> ->
	    [LEFT, RIGHT, TOP, BOTTOM, _]  = egN(Rest4, 4),
	    SPS1#sps{
	      frame_mbs_only_flag          = 0,
	      mb_adaptive_frame_field_flag = MB_ADAPTIVE_FRAME_FIELD_FLAG,
	      direct_8x8_inference_flag	   = DIRECT_8x8_INFERENCE_FLAG,
	      frame_cropping_flag          = 1,
	      frame_crop_offsets           = [LEFT, RIGHT, TOP, BOTTOM]};
	<<0:1, MB_ADAPTIVE_FRAME_FIELD_FLAG:1, DIRECT_8x8_INFERENCE_FLAG:1, 0:1, _Rest4/bitstring>> ->
	    SPS1#sps{
	      frame_mbs_only_flag          = 0,
	      mb_adaptive_frame_field_flag = MB_ADAPTIVE_FRAME_FIELD_FLAG,
	      direct_8x8_inference_flag	   = DIRECT_8x8_INFERENCE_FLAG,
	      frame_cropping_flag          = 0};
	<<1:1, DIRECT_8x8_INFERENCE_FLAG:1, 1:1, Rest4/bitstring>> ->
	    [LEFT, RIGHT, TOP, BOTTOM, _]  = egN(Rest4, 4),
	    SPS1#sps{
	      frame_mbs_only_flag          = 1,
	      direct_8x8_inference_flag    = DIRECT_8x8_INFERENCE_FLAG,
	      frame_cropping_flag          = 1,
	      frame_crop_offsets           = [LEFT, RIGHT, TOP, BOTTOM]};
	<<1:1, DIRECT_8x8_INFERENCE_FLAG:1, 0:1, _Rest4/bitstring>> ->
	    SPS1#sps{
	      frame_mbs_only_flag          = 0,
	      direct_8x8_inference_flag	   = DIRECT_8x8_INFERENCE_FLAG,
	      frame_cropping_flag          = 0}
	end.    

%% source http://stackoverflow.com/questions/6394874/fetching-the-dimensions-of-a-h264video-stream
old_parseSPS(Data) ->
    <<PROFILE_IDC:8, _CS0:1, _CS1:1, _CS2:1, _CS3:1, _CS4:1, 0:3, LEVEL_IDC:8, Rest/bitstring>> = Data,
    [SEQ_PARAM_SET_ID,
     _LOG2_MAX_FRAME_NUM_MINUS4,
     PIC_ORDER_CNT_TYPE, %% =:= 0
     LOG2_MAX_PIC_ORDER_CNT_LSB_MINUS4,
     MAX_NUM_REF_FRAMES,
     Rest1] = egN(Rest, 5),    
    <<Gaps:1, Rest2/bitstring>> = Rest1,
    io:format("~w~n",[Rest2]),
    [PIC_WIDTH_IN_MBS_MINUS_1,
     PIC_HEIGHT_IN_MAP_MINUS_1,
     Rest3] = egN(Rest2, 2),
    <<FRAME_MBS_ONLY:1, DIRECT_8x8_INFERENCE:1, FRAME_CROPPING:1, _Rest4/bitstring>> = Rest3,    
    [{profile_idc, PROFILE_IDC},
     {level_idc, LEVEL_IDC},
     {seq_param_set_id, SEQ_PARAM_SET_ID},
     {pic_order_cnt_type, PIC_ORDER_CNT_TYPE},
     {log2_max_pic_ORDER_CNT_LSB_MINUS4,  LOG2_MAX_PIC_ORDER_CNT_LSB_MINUS4},
     {max_num_ref_frames, MAX_NUM_REF_FRAMES},
     {gaps, Gaps},
     {pic_width_in_mbs_minus_1, PIC_WIDTH_IN_MBS_MINUS_1}, 
     {pic_height_in_map_minus_1, PIC_HEIGHT_IN_MAP_MINUS_1},
     {frame_mbs_only, FRAME_MBS_ONLY},
     {direct_8x8_inference, DIRECT_8x8_INFERENCE}, 
     {frame_cropping, FRAME_CROPPING}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ex_desc(<<>>) ->
    <<"">>;
ex_desc(<<T, L, Desc:L/binary>>) ->
    {desc_type(T), Desc};
ex_desc(D) ->
    {raw, D}.


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
stype2str(22)  -> metadata_in_metadata_sections;	%"Metadata carried in metadata_sections"
stype2str(27)  -> h264_video;
stype2str(129) -> atsc_AC3_audio;			%"ATSC AC-3 Audio";
stype2str(_)   -> mpeg2_user_private.
