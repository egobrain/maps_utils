-module(maps_utils).

%% API exports
-export([
         rename_key/3,
         rename_keys/2,
         map_and_rename/2,
         merge_with/3,
         filter/2,
         filtermap/2,
         diff/2,
         update/4,
         apply_diff/2,
         diff_and_apply/2
        ]).

-type path_el()       :: {Key :: term(), list | map}.
-type path()          :: list(path_el()).
-type diff_operator() :: map().

%%==============================================================================
%% API functions
%%==============================================================================

diff_and_apply(Val1, Val2) ->
    Diff = diff(Val1, Val2),
    apply_diff(Val1, Diff).

-spec rename_key(OldName, NewName, Map) -> Res when
      OldName :: term(),
      NewName :: term(),
      Map :: map(),
      Res :: map().
rename_key(OldName, NewName, Map) ->
    maps:put(NewName, maps:get(OldName, Map), maps:remove(OldName, Map)).

-spec rename_keys(RenameFun, Map) -> Res when
      RenameFun :: fun(),
      Map :: map(),
      Res :: map().
rename_keys(RenameFun, Map) ->
    maps:fold(fun(OldK, V, M) ->
        maps:put(RenameFun(OldK), V, M)
    end, #{}, Map).

-spec map_and_rename(Fun, Map) -> Res when
      Fun :: fun(),
      Map :: map(),
      Res :: map().
map_and_rename(Fun, Map) ->
    maps:fold(fun(OldK, OldV, M) ->
        {NewK, NewV} = Fun(OldK, OldV),
        maps:put(NewK, NewV, M)
    end, #{}, Map).

-spec merge_with(Fun, M1, M2) -> Res when
      Fun :: fun(),
      M1 :: map(),
      M2 :: map(),
      Res :: map().
merge_with(Fun, M1, M2) ->
    maps:fold(fun(K1, V1, M) ->
        maps:put(K1, case maps:find(K1, M2) of
            {ok, V2} -> Fun(V1, V2);
            error -> V1
        end, M)
    end, M2, M1).

-spec filter(Fun, Map) -> Res when
      Fun :: fun(),
      Map :: map(),
      Res :: map().
filter(Fun, Map) ->
    maps:fold(fun(K, V, M) ->
        case Fun(K, V) of
            true -> M;
            false -> maps:remove(K, M)
        end
    end, Map, Map).

-spec filtermap(Fun, Map) -> Res when
      Fun :: fun(),
      Map :: map(),
      Res :: map().
filtermap(Fun, Map) ->
    maps:fold(fun(K, V, M) ->
        case Fun(K, V) of
            {true, NewV} -> maps:put(K, NewV, M);
            false -> M
        end
    end, #{}, Map).

-spec diff(From, To) -> Res when
      From :: map(),
      To :: map(),
      Res :: list(diff_operator()).
diff(From, To) ->
    lists:reverse(diff(From, To, [], [])).

-spec diff(From, To, Path, Log) -> Res when
      From :: map() | list(),
      To :: map() | list(),
      Path :: path(),
      Log :: list(),
      Res :: list(diff_operator()).
diff(From, To, Path, Log) when is_map(From), is_map(To) ->
    FromKeys = maps:keys(From),
    NewPairs = maps:to_list(maps:without(FromKeys, To)),
    {Log2, NewPairs2} = maps:fold(
        fun(K, FromV, {L, New}) ->
            case maps:find(K, To) of
                {ok, ToV} -> {diff(FromV, ToV, [{K, map} | Path], L), New};
                error -> maybe_moved(K, FromV, NewPairs, Path, L)
            end
        end, {Log, NewPairs}, From),
    lists:foldl(fun({K, V}, L) ->
        [#{op => add, path => path([{K, map} | Path]), value => V}|L]
    end, Log2, NewPairs2);
diff(From, To, Path, Log) when is_list(From), is_list(To) ->
    list_diff(From, To, Path, Log, 0);
diff(From, To, _Path, Log) when From =:= To -> Log;
diff(_From, To, Path, Log) ->
    [#{op => replace, path => path(Path), value => To}|Log].

-spec update(Key, Map, New, UpdateFun) -> Res when
      Key :: term(),
      Map :: map(),
      New :: term(),
      UpdateFun :: fun(),
      Res :: map().
update(Key, Map, New, UpdateFun) ->
    maps:put(Key,
        case maps:find(Key, Map) of
            {ok, V} -> UpdateFun(V);
            error -> New
        end, Map).

-spec apply_diff(Data, Diff) -> Res when
      Data :: term(),
      Diff :: list(diff_operator()),
      Res :: term().
apply_diff(Data, Diff) ->
    lists:foldl(fun apply_op/2, Data, Diff).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec apply_op(Diff, Data) -> Res when
      Diff :: diff_operator(),
      Data :: term(),
      Res :: term().
apply_op(#{op := Op, path := Path} = Diff, Data) ->
    NewVal = maps:get(value, Diff, undefined),
    apply_op(Data, Op, Path, NewVal).

-spec apply_op(Data, Op, Path, NewValue) -> Res when
      Data :: term(),
      Op :: diff_operator(),
      Path :: path(),
      NewValue :: term(),
      Res :: term().
apply_op(_Data, _Op, [], NewVal) ->
    NewVal;
apply_op(Data, remove, [{Index, list}], _NewVal) when is_list(Data) ->
    rm_list_el(Data, Index);
apply_op(Data, remove, [{Key, map}], _NewVal) when is_map(Data) ->
    maps:remove(Key, Data);
apply_op(Data, Op, [{Index, list} | T], NewVal) when is_list(Data) ->
    ListEl = nth_el(Data, Index, T, NewVal),
    RemainingEls = rm_list_el(Data, Index),
    NewEl = apply_op(ListEl, Op, T, NewVal),
    add_list_el(RemainingEls, Index, NewEl);
apply_op(Data, Op, [{Key, map} | T], NewVal) when is_map(Data) ->
    MapValue = map_value(Data, Key, T, NewVal),
    NewMapVal = apply_op(MapValue, Op, T, NewVal),
    maps:put(Key, NewMapVal, Data).

-spec map_value(Map, Key, Path, NewVal) -> Res when
      Map :: map(),
      Key :: term(),
      Path :: path(),
      NewVal :: term(),
      Res :: term().
map_value(Map, Key, Path, NewVal) ->
    maps:get(Key, Map, def_value(Path, NewVal)).

-spec add_list_el(List, Index, El) -> Res when
      List :: list(),
      Index :: non_neg_integer() | '-',
      El :: term(),
      Res :: list().
add_list_el(List, '-', El) ->
    List ++ [El];
add_list_el(List, Ind0, El) ->
    Ind = Ind0 + 1,
    FirstEls = string:substr(List, 1, Ind - 1),
    LastEls = string:substr(List, Ind),
    FirstEls ++ [El] ++ LastEls.

-spec rm_list_el(List, Index) -> Res when
      List :: list(),
      Index :: non_neg_integer() | '-',
      Res :: list().
rm_list_el(List, '-') ->
    List;
rm_list_el(List, Ind0) ->
    Ind = Ind0 + 1,
    [E ||
     {E, I} <- lists:zip(List, lists:seq(1, length(List))), I =/= Ind].

-spec nth_el(List, Index, Path, NewVal) -> Res when
      List :: list(),
      Index :: non_neg_integer() | '-',
      Path :: path(),
      NewVal :: term(),
      Res :: term().
nth_el(_List, '-', _Path, NewVal) ->
    NewVal;
nth_el(List, Ind0, Path, NewVal) ->
    Ind = Ind0 + 1,
    case length(List) < Ind of
        true ->
            def_value(Path, NewVal);
        false ->
            lists:nth(Ind, List)
    end.

-spec def_value(Path, NewVal) -> Res when
      Path :: path(),
      NewVal :: term(),
      Res :: term().
def_value([{_, list} | _], _NewVal) ->
    [];
def_value([{_, map} | _], _NewVal) ->
    #{};
def_value([], NewVal) ->
    NewVal.

-spec maybe_moved(K, FromV, Pairs, Path, Log) -> Res when
      K :: term(),
      FromV :: term(),
      Pairs :: proplists:proplist(),
      Path :: path(),
      Log :: list(),
      Res :: term().
maybe_moved(K, FromV, Pairs, Path, L) ->
    maybe_moved_(K, FromV, Pairs, Path, L, []).
maybe_moved_(K, _V, [], Path, Log, Acc) ->
    {[#{op => remove, path => path([{K, map} | Path])}|Log], Acc};
maybe_moved_(K, V, [{NewK, V}|Rest], Path, Log, Acc) ->
    {[#{op => move, path => path([{NewK, map} | Path]), from => path([{K, map} | Path])}|Log],
     Acc ++ Rest};
maybe_moved_(K, V, [Other|Rest], Path, Log, Acc) ->
    maybe_moved_(K, V, Rest, Path, Log, [Other|Acc]).

-spec list_diff(From, To, Path, Log, Counter) -> Res when
      From :: list(),
      To :: list(),
      Path :: path(),
      Log :: list(),
      Counter :: non_neg_integer() | '-',
      Res :: list(diff_operator()).
list_diff([From|RestF], [To|RestT], Path, Log, Cnt) ->
    list_diff(RestF, RestT, Path, diff(From, To, [{Cnt, list}|Path], Log), Cnt+1);
list_diff([_|Rest], [], Path, Log, Cnt) ->
    NewLog = [#{op => remove, path => path([{Cnt, list}|Path])}|Log],
    list_diff(Rest, [], Path, NewLog, Cnt+1);
list_diff([], Rest, Path, Log, _Cnt) ->
    lists:foldl(fun(V, L) ->
        [#{op => add, path => path([{'-', list} | Path]), value => V}|L]
    end, Log, Rest).

-spec path(Path) -> Res when
      Path :: path(),
      Res :: path().
path(Path) ->
    lists:reverse(Path).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rename_key_test() ->
    ?assertEqual(#{b => 1}, rename_key(a, b, #{a => 1})).

rename_keys_test() ->
    ?assertEqual(
        #{"a" => 1, "b" => 2},
        rename_keys(fun erlang:atom_to_list/1, #{a => 1, b => 2})).

map_and_rename_test() ->
    ?assertEqual(
        #{"a" => 2, "b" => 4},
        map_and_rename(
            fun(K, V) -> {atom_to_list(K), V * 2} end,
            #{a => 1, b => 2})).

filter_test() ->
    ?assertEqual(
         #{a => 1, b => 2},
         filter(
             fun(_, V) -> V < 3 end,
             #{a => 1, b => 2, c => 3, d => 4})).

filtermap_test_() ->
    [
     ?_test(?assertEqual(#{}, filtermap(fun(_K, V) -> {true, V} end, #{}))),
     ?_test(?assertEqual(#{}, filtermap(fun(_K, _V) -> false end, #{a => 1, b => 2}))),
     ?_test(?assertEqual(
         #{a => 2, c => 6},
         filtermap(fun(_K, V) ->
             case V rem 2 of
                 1 -> {true, V*2};
                 _ -> false
             end
         end, #{a => 1, b => 2, c => 3})))
    ].

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

merge_with_test_() ->
    [
     ?_test(?assertEqual(#{a => 1}, merge_with(fun(A, B) -> A+B end, #{}, #{a => 1}))),
     ?_test(?assertEqual(#{a => 1}, merge_with(fun(A, B) -> A+B end, #{a => 1}, #{}))),
     ?_test(?assertEqual(#{a => 1, b => 1}, merge_with(fun(A, B) -> A+B end, #{a => 1}, #{b => 1}))),
     ?_test(?assertEqual(#{a => 3}, merge_with(fun(A, B) -> A+B end, #{a => 1}, #{a => 2})))
    ].

update_test_() ->
    [
     ?_test(?assertEqual(#{a => 1}, update(a, #{}, 1, fun(A) -> A+2 end))),
     ?_test(?assertEqual(#{a => 5}, update(a, #{a => 3}, 1, fun(A) -> A+2 end)))
    ].

-endif.
