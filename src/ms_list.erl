-module(ms_list).
-export([chunk/2]).

chunk(List, N) ->
    chunk(List, N, []).

chunk([], _, Acc) ->
    Acc;
chunk(List, N, Acc) ->    
    {Chunk, Rest} = split2(N, List, []),
    chunk(Rest, N, [Chunk | Acc]).

split2(0, Rest, Acc) ->
    {Acc, Rest};
split2(_, [], Acc) ->
    {Acc, []};
split2(N, [X|XS], Acc) ->
    split2(N-1, XS, [X|Acc]).    
