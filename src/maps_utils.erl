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
         diff/3,
         update/4
        ]).

%%====================================================================
%% API functions
%%====================================================================

rename_key(OldName, NewName, Map) ->
    maps:put(NewName, maps:get(OldName, Map), maps:remove(OldName, Map)).

rename_keys(RenameFun, Map) ->
    maps:fold(fun(OldK, V, M) ->
        maps:put(RenameFun(OldK), V, M)
    end, #{}, Map).

map_and_rename(Fun, Map) ->
    maps:fold(fun(OldK, OldV, M) ->
        {NewK, NewV} = Fun(OldK, OldV),
        maps:put(NewK, NewV, M)
    end, #{}, Map).

merge_with(Fun, M1, M2) ->
    maps:fold(fun(K1, V1, M) ->
        maps:put(K1, case maps:find(K1, M2) of
            {ok, V2} -> Fun(V1, V2);
            error -> V1
        end, M)
    end, M2, M1).

filter(Fun, Map) ->
    maps:fold(fun(K, V, M) ->
        case Fun(K, V) of
            true -> M;
            false -> maps:remove(K, M)
        end
    end, Map, Map).

filtermap(Fun, Map) ->
    maps:fold(fun(K, V, M) ->
        case Fun(K, V) of
            {true, NewV} -> maps:put(K, NewV, M);
            false -> M
        end
    end, #{}, Map).

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
diff(From, To, Path, Log) when is_list(From), is_list(To) ->
  list_diff(From, To, Path, Log, 0);
diff(From, To, _Path, Log) when From =:= To -> Log;
diff(_From, To, Path, Log) ->
  [#{op => replace, path => path(Path), value => To}|Log].

diff(From, To, Fun) ->
    lists:reverse(diff(From, To, [], [], Fun)).

diff(From, To, Path, Log, Fun) when is_map(From), is_map(To) ->
  FromKeys = maps:keys(From),
  NewPairs = maps:to_list(maps:without(FromKeys, To)),
  {Log2, NewPairs2} = maps:fold(
    fun(K, FromV, {L, New}) ->
      case maps:find(K, To) of
        {ok, ToV} ->
          {diff(FromV, ToV, [K | Path], L, Fun), New};
        error ->
          maybe_moved(K, FromV, NewPairs, Path, L)
      end
    end, {Log, NewPairs}, From),
  lists:foldl(fun({K, V}, L) ->
                [#{op => add, path => path([K | Path]), value => V} | L]
              end, Log2, NewPairs2);
diff(From, To, Path, Log, Fun) when is_list(From), is_list(To) ->
  list_diff(From, To, Path, Log, 0, Fun);
diff(From, To, _Path, Log, _Fun) when From =:= To ->
  Log;
diff(From, To, Path, Log, Fun) when is_binary(From), is_binary(To) ->
  Fun(From, To, path(Path), Log);
diff(From, To, Path, Log, Fun) when is_integer(From), is_integer(To), To < From ->
  Fun(From, To, path(Path), Log);
diff(From, To, Path, Log, Fun) when is_integer(From), is_integer(To), To > From ->
  Fun(From, To, path(Path), Log);
diff(_From, To, Path, Log, _Fun) ->
  [#{op => replace, path => path(Path), value => To}|Log].

update(Key, Map, New, UpdateFun) ->
    maps:put(Key,
        case maps:find(Key, Map) of
            {ok, V} -> UpdateFun(V);
            error -> New
        end, Map).

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

list_diff([From|RestF], [To|RestT], Path, Log, Cnt, Fun) ->
    list_diff(RestF, RestT, Path, diff(From, To, [Cnt|Path], Log, Fun), Cnt+1, Fun);
list_diff([_|Rest], [], Path, Log, Cnt, Fun) ->
    NewLog = [#{op => remove, path => path([Cnt|Path])}|Log],
    list_diff(Rest, [], Path, NewLog, Cnt+1, Fun);
list_diff([], Rest, Path, Log, _Cnt, Fun) ->
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

diff_nums_test_() ->
  Fun = fun(_, To, _, Log) -> [#{fun_result => To} | Log] end,
  [
    ?_test(?assertEqual([], diff(#{a => 1}, #{a => 1}, Fun))),
    ?_test(?assertEqual([#{fun_result => 2}], diff(#{a => 1}, #{a => 2}, Fun))),
    ?_test(?assertEqual(
      [
        #{ op => move, path => <<"/b">>, from => <<"/a">>}
      ],
      diff(#{a => 1}, #{b => 1}, Fun))),
    ?_test(?assertEqual(
      [
        #{ op => add, path => <<"/a">>, value => 1}
      ],
      diff(#{}, #{a => 1}, Fun))),
    ?_test(?assertEqual(
      [
        #{ op => remove, path => <<"/a">>}
      ],
      diff(#{a => 1}, #{}, Fun))),
    ?_test(?assertEqual(
      [
        #{op => remove,path => <<"/a">>},
        #{op => add,path => <<"/c">>,value => 3},
        #{op => add,path => <<"/b">>,value => 2}
      ],
      diff(#{a => 1}, #{b => 2, c => 3}, Fun))),
    ?_test(?assertEqual(
      [#{from => <<"/a">>,op => move,path => <<"/e">>},
       #{fun_result => 2},
       #{fun_result => 4},
       #{op => add,path => <<"/b/-">>,value => 7},
       #{op => add,path => <<"/k">>,value => #{l => 1}}],
      diff(#{a => 1, b => [1, #{c => 3}], d => 4},
           #{e => 1, b => [2, #{c => 4}, 7], d => 4, k => #{l => 1}}, Fun))),

    ?_test(?assertEqual(
      [#{fun_result => 2},
       #{op => replace,path => <<"/b">>,value => y},
       #{fun_result => 3},
       #{fun_result => <<"3">>},
       #{op => remove,path => <<"/e/1">>}],
      diff(#{a => 1, b => x, c => 4, d => <<"4">>, e => [1, 2]},
           #{a => 2, b => y, c => 3, d => <<"3">>, e => [1]}, Fun)))
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
