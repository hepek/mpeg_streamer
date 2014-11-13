%%%-------------------------------------------------------------------
%%% @author  <milan@epikur>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2014 by  <milan@epikur>
%%%-------------------------------------------------------------------
-module(mpeg_streamer_server).

-behaviour(gen_server).

%% API
-export([start_link/0, 
	 info/1,
	 shutdown/0,
	 start_streamer/2,
	 stop_streamer/1,
	 list_streamers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {streamers=[],
	        nextId = 0}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% API

info(MediaSourceURL) ->
    gen_server:call(?SERVER, {info, MediaSourceURL}).

shutdown() ->
    gen_server:cast(?SERVER, stop).

start_streamer(MediaSourceURL, DestinationURL) ->
    gen_server:call(?SERVER, {start_stream, MediaSourceURL, DestinationURL}).

stop_streamer(ID) ->
    gen_server:call(?SERVER, {stop_stream, ID}).

list_streamers() ->
    gen_server:call(?SERVER, list_streamers).

%% Gen Server API

init([]) ->
    {ok, #state{}}.

handle_call(list_streamers, _From, State) ->
    {reply, State#state.streamers, State};

handle_call({start_stream, SRC, DST}, _From, State) ->
    Pid = spawn(udp_streamer, init, [self(), make_ref(), SRC, DST]),
    receive
	{Ref, ok} ->
	    Id = State#state.nextId,
	    Streamers = [{Id, Pid, SRC, DST} | State#state.streamers],
	    {reply, Id, State#state{streamers=Streamers, nextId=Id+1}};
	{Ref, Error} ->
	    error_logger:error_msg("start_stream: error: ~p~n", [Error]),
	    {reply, {error, Error}, State}
    after 5000 ->
	    {reply, timeout, State}
    end;

handle_call({stop_stream, ID}, _From, State) ->
    case lists:keytake(ID, 1, State#state.streamers) of
	false ->
	    {reply, no_such_stream, State};
	{value, {_, Pid, _, _}, Streamers2} ->
	    {ok, _} = udp_streamer:stop(Pid),
	    {reply, ok, State#state{streamers=Streamers2}}
    end;
handle_call({info, Source}, _From, State) ->   
    case (catch stream_info:info(Source)) of
	{'EXIT', Reason} ->
	    {reply, {error, Reason}, State};
	Info                -> 
	    {reply, Info, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    error_logger:warning_msg("Unexpected message ~p~n", Msg),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
