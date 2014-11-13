-module(uri_utils).
-export([open/1, open/2, parse/1, scheme_def/0]).

-include_lib("kernel/include/inet.hrl").

parse(URI) ->
    case http_uri:parse(URI, ?MODULE:scheme_def()) of
	{ok, {file, _, _, _, FName, _}} ->
	    {file_name, FName};
	{ok, {udp, _, Host, Port, _, _}} ->
	    {ok, H} = inet:gethostbyname(Host),
	    Addr = hd(H#hostent.h_addr_list),
	    {udp_addr, Addr, Port};
	E -> {error, E}
	       
    end.

scheme_def() ->
     [{scheme_defaults, [{file,0} | http_uri:scheme_defaults()]}].

open(URI, sender) -> 
    case parse(URI) of
	{file_name, FName} ->
	    case file:open(FName, [binary, read]) of		
		{ok, File} -> {file, File};
		_ -> exit({error, no_such_file, FName})
	    end;
	{udp_addr, Addr, _Port} ->
	    Opts = get_opts(Addr),
	    {ok, Sock} = gen_udp:open(0, Opts),
	    {udp, Sock};
	_ -> {error, cannot_parse_uri, URI}
    end.

open(URI) -> 
    case parse(URI) of
	{file_name, FName} ->
	    case file:open(FName, [binary, read]) of		
		{ok, File} -> {file, File};
		_ -> exit({error, no_such_file, FName})
	    end;
	{udp_addr, Addr, Port} ->
	    Opts = get_opts(Addr),
	    {ok, Sock} = gen_udp:open(Port, Opts),
	    {udp, Sock};
	_ -> {error, cannot_parse_uri, URI}
    end.

get_opts(Addr = {A, _, _, _}) when (A >= 224 andalso A =< 239) ->
    [binary, {active, false}, 
     {add_membership, {Addr, {0,0,0,0}}},
     {multicast_ttl, 2},
     {reuseaddr, true}];
get_opts(_) ->
    [binary, {active, false}].
