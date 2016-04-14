-module(maps_utils).

%% API exports
-export([diff/2]).

%%====================================================================
%% API functions
%%====================================================================

diff(From, To) ->
    lists:reverse(diff(From, To, [], [])).

diff(From, To, Path, Log) when is_map(From), is_map(To) ->
    FromKeys = maps:keys(From),
    NewPairs = maps:to_list(maps:without(FromKeys, To)),
    {Log2, NewPairs2} = maps:fold(
        fun(K, FromV, {L, New}) ->
            case maps:find(K, To) of
                {ok, ToV} -> {diff(FromV, ToV, [K|Path], L), New};
                error -> maybe_moved(K, FromV, NewPairs, Path, L)
            end
        end, {Log, NewPairs}, From),
    lists:foldl(fun({K, V}, L) ->
        [#{op => add, path => path([K|Path]), value => V}|L]
    end, Log2, NewPairs2);
diff(From, To, Path, Log) when is_list(From); is_list(To) ->
    list_diff(From, To, Path, Log, 0);
diff(From, To, _Path, Log) when From =:= To -> Log;
diff(_From, To, Path, Log) ->
    [#{op => replace, path => path(Path), value => To}|Log].

%%====================================================================
%% Internal functions
%%====================================================================

maybe_moved(K, FromV, Pairs, Path, L) ->
    maybe_moved_(K, FromV, Pairs, Path, L, []).
maybe_moved_(K, _V, [], Path, Log, Acc) ->
    {[#{op => remove, path => path([K|Path])}|Log], Acc};
maybe_moved_(K, V, [{NewK, V}|Rest], Path, Log, Acc) ->
    {[#{op => move, path => path([NewK|Path]), from => path([K|Path])}|Log],
     Acc ++ Rest};
maybe_moved_(K, V, [Other|Rest], Path, Log, Acc) ->
    maybe_moved_(K, V, Rest, Path, Log, [Other|Acc]).

list_diff([From|RestF], [To|RestT], Path, Log, Cnt) ->
    list_diff(RestF, RestT, Path, diff(From, To, [Cnt|Path], Log), Cnt+1);
list_diff([_|Rest], [], Path, Log, Cnt) ->
    NewLog = [#{op => remove, path => path([Cnt|Path])}|Log],
    list_diff(Rest, [], Path, NewLog, Cnt+1);
list_diff([], Rest, Path, Log, _Cnt) ->
    lists:foldl(fun(V, L) ->
        [#{op => add, path => path(["-"|Path]), value => V}|L]
    end, Log, Rest).

path(Path) ->
    iolist_to_binary([["/", to_iodata(P)] || P <- lists:reverse(Path)]).

to_iodata(P) when is_atom(P) -> atom_to_list(P);
to_iodata(P) when is_integer(P) -> integer_to_list(P);
to_iodata(P) -> P.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

diff_test_() ->
    [
     ?_test(?assertEqual([], diff(#{a => 1}, #{a => 1}))),
     ?_test(?assertEqual(
         [
          #{ op => replace, path => <<"/a">>, value => 2}
         ],
         diff(#{a => 1}, #{a => 2}))),
     ?_test(?assertEqual(
         [
          #{ op => move, path => <<"/b">>, from => <<"/a">>}
         ],
         diff(#{a => 1}, #{b => 1}))),
     ?_test(?assertEqual(
         [
          #{ op => add, path => <<"/a">>, value => 1}
         ],
         diff(#{}, #{a => 1}))),
     ?_test(?assertEqual(
         [
          #{ op => remove, path => <<"/a">>}
         ],
         diff(#{a => 1}, #{}))),
     ?_test(?assertEqual(
         [
          #{op => remove,path => <<"/a">>},
          #{op => add,path => <<"/c">>,value => 3},
          #{op => add,path => <<"/b">>,value => 2}
         ],
         diff(#{a => 1}, #{b => 2, c => 3}))),
     ?_test(?assertEqual(
         [
          #{op => move,path => <<"/e">>,from => <<"/a">>},
          #{op => replace,path => <<"/b/0">>,value => 2},
          #{op => replace,path => <<"/b/1/c">>,value => 4},
          #{op => add,path => <<"/b/-">>,value => 7},
          #{op => add,path => <<"/k">>,value => #{l => 1}}
         ],
         diff(#{a => 1, b => [1, #{c => 3}], d => 4},
              #{e => 1, b => [2, #{c => 4}, 7], d => 4, k => #{l => 1}}))),

     ?_test(?assertEqual(
         [
          #{op => move,path => <<"/e">>,from => <<"/a">>},
          #{op => replace,path => <<"/b/0">>,value => 2},
          #{op => replace,path => <<"/b/1/c">>,value => 4},
          #{op => remove,path => <<"/b/2">>},
          #{op => add,path => <<"/k">>,value => #{l => 1}}
         ],
         diff(#{a => 1, b => [1, #{c => 3}, 7], d => 4},
              #{e => 1, b => [2, #{c => 4}], d => 4, k => #{l => 1}})))
    ].

-endif.
