%% -*- erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%------------------------------------------------------------------------------

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
         update/4,
         apply_diff/2,
         apply_diff/3,
         revert_diff/2
        ]).

%% exported for eunit only
-export([get_val/2,
         sort_remove_operators/1,
         revert_op/1]).

-type op()            :: move | rename | remove | add.
-type path_el()       :: {Key :: term(), list | map}.
-type path()          :: list(path_el()).
-type diff_operator() :: #{path => path(), value => term(), op => op(),
                           from => path()}.
-type index()         :: non_neg_integer() | '-'.

%%==============================================================================
%% API functions
%%==============================================================================

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
        [#{op => add, path => path([{K, map} | Path]), value => V} | L]
    end, Log2, NewPairs2);
diff(From, To, Path, Log) when is_list(From), is_list(To) ->
  list_diff(From, To, Path, Log, 0);
diff(From, To, _Path, Log) when From =:= To ->
    Log;
diff(From, To, Path, Log) ->
  [#{op => replace, path => path(Path), value => To, orig_value => From}|Log].


%% TODO handle move operator in revert
%% TODO check move operator in apply_diff

-spec diff(From, To, UpdateFun) -> Res when
      From :: map(),
      To :: map(),
      UpdateFun :: fun((From :: map(), To :: map(),
                        Path :: path(), Log :: list()) -> map()),
      Res :: list(diff_operator()).
diff(From, To, Fun) ->
    lists:reverse(diff(From, To, [], [], Fun)).

-spec diff(From, To, Path, Log, UpdateFun) -> Res when
      From :: map(),
      To :: map(),
      Path :: path(),
      Log :: list(),
      UpdateFun :: fun((From :: map(), To :: map(),
                        Path :: path(), Log :: list()) -> map()),
      Res :: list(diff_operator()).
diff(From, To, Path, Log, Fun) when is_map(From), is_map(To) ->
  FromKeys = maps:keys(From),
  NewPairs = maps:to_list(maps:without(FromKeys, To)),
  {Log2, NewPairs2} = maps:fold(
    fun(K, FromV, {L, New}) ->
      case maps:find(K, To) of
        {ok, ToV} ->
          {diff(FromV, ToV, [{K, map} | Path], L, Fun), New};
        error ->
          maybe_moved(K, FromV, NewPairs, Path, L)
      end
    end, {Log, NewPairs}, From),
  lists:foldl(fun({K, V}, L) ->
                [#{op => add, path => path([{K, map} | Path]), value => V} | L]
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
diff(From, To, Path, Log, _Fun) ->
  [#{op => replace, path => path(Path), value => To, orig_value => From} | Log].

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
apply_diff(Data, Diff0) ->
    Diff = sort_remove_operators(Diff0),
    lists:foldl(fun apply_op/2, Data, Diff).

-spec apply_diff(Data, Diff, Fun) -> Res when
      Data :: term(),
      Diff :: list(diff_operator()),
      Fun :: fun(),
      Res :: term().
apply_diff(Data, Diff0, Fun) ->
    Diff = sort_remove_operators(Diff0),
    lists:foldl(fun(Op, Acc) ->
                        apply_op(Op, Acc, Fun)
                end, Data, Diff).

-spec revert_diff(Data, Diff) -> Res when
      Data :: term(),
      Diff :: list(diff_operator()),
      Res :: term().
revert_diff(Data, Diff) ->
    RevertedDiff = reverted_diff(Diff),
    apply_diff(Data, RevertedDiff).

%%==============================================================================
%% Exported for eunit
%%==============================================================================

-spec get_val(Data, Path) -> Res when
      Data :: term(),
      Path :: path(),
      Res :: term().
get_val(Lst, [{Idx, list}]) when is_list(Lst) ->
    lists:nth(Idx + 1, Lst);
get_val(Map, [{Key, map}]) when is_map(Map) ->
    maps:get(Key, Map);
get_val(Lst, [{Idx, list} | T]) when is_list(Lst) ->
    get_val(lists:nth(Idx + 1, Lst), T);
get_val(Map, [{Key, map} | T]) when is_map(Map) ->
    get_val(maps:get(Key, Map), T).

-spec sort_remove_operators(Diff) -> Res when
      Diff :: list(diff_operator()),
      Res :: list(diff_operator()).
sort_remove_operators(Diff) ->
    filtered_sort(Diff, fun is_remove_operator/1,
                  fun compare_remove_operators/2).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec reverted_diff(Diff) -> Res when
      Diff :: list(diff_operator()),
      Res :: list(diff_operator()).
reverted_diff(Diff) ->
    [revert_op(Op) || Op <- Diff].

-spec revert_op(Op) -> Res when
      Op :: diff_operator(),
      Res :: diff_operator().
revert_op(#{op := add} = Op) ->
    Op#{op => remove};
revert_op(#{op := remove, orig_value := Orig, path := Path} = Op) ->
    ConvertedPath = convert_revert_path(Path, []),
    Op#{op => add, value => Orig, path => ConvertedPath};
revert_op(#{op := replace, orig_value := Orig, value := Value} = Op) ->
    Op#{orig_value => Value, value => Orig};
revert_op(#{op := move, from := From, path := Path} = Op) ->
    Op#{from => Path, path => From};
revert_op(Op) ->
    Op.

-spec convert_revert_path(Path, Acc) -> Res when
      Path :: path(),
      Acc :: path(),
      Res :: path().
convert_revert_path([{_, list}], Acc) ->
    lists:reverse([{'-', list} | Acc]);
convert_revert_path([El], Acc) ->
    lists:reverse([El | Acc]);
convert_revert_path([El | T], Acc) ->
    convert_revert_path(T, [El | Acc]).

%%
%% Sort only filtered elements of the list. FilterFun is used to select
%% some matching elements. Those selected elements will be sorted. Order
%% of other elements will remain the same. For example sort only positive numbers:
%% filtered_sort([0, 2, 0, 1, 0, 3],
%%               fun(A) -> A > 0 end,
%%               fun(A, B) -> A < B end) ->
%%     [0, 1, 0, 2, 0, 3]
%%
-spec filtered_sort(List, FilterFun, SortFun) -> Res when
      List :: list(),
      FilterFun :: fun((term()) -> boolean()),
      SortFun :: fun((term(), term()) -> boolean()),
      Res :: list().
filtered_sort(Lst, FilterFun, SortFun) ->
  Length = length(Lst),
  IndexedLst = lists:zip(lists:seq(1, Length), Lst),
  Filtered = lists:filter(fun({_Idx, El}) -> FilterFun(El) end,
                          IndexedLst),
  {FilteredIdxs, FilteredEls} = lists:unzip(Filtered),
  Sorted = lists:sort(SortFun, FilteredEls),
  FilteredWithIdxs = lists:zip(FilteredIdxs, Sorted),
  Remaining = IndexedLst -- Filtered,
  AllSorted = lists:sort(fun({Idx1, _}, {Idx2, _}) -> Idx1 < Idx2 end,
                         FilteredWithIdxs ++ Remaining),
  {_, Result} = lists:unzip(AllSorted),
  Result.

-spec is_remove_operator(Op) -> Res when
      Op :: diff_operator(),
      Res :: boolean().
is_remove_operator(#{op := remove}) ->
    true;
is_remove_operator(_Op) ->
    false.

-spec compare_remove_operators(Op1, Op2) -> Res when
      Op1 :: diff_operator(),
      Op2 :: diff_operator(),
      Res :: boolean().
compare_remove_operators(#{path := Path1}, #{path := Path2}) ->
    Path1 > Path2.

-spec apply_op(Diff, Data) -> Res when
      Diff :: diff_operator(),
      Data :: term(),
      Res :: term().
apply_op(Diff, Data) ->
    apply_op(Diff, Data, undefined).

-spec apply_op(Diff, Data, Fun) -> Res when
      Diff :: diff_operator(),
      Data :: term(),
      Fun :: undefined | fun((diff_operator()) -> term()),
      Res :: term().
apply_op(#{op := Op, path := Path} = Diff, Data, Fun) ->
    NewVal0 = get_new_val(Diff, Data),
    NewVal = apply_update_fun(Fun, Diff, NewVal0, Data, Path),
    NewData = apply_op(Data, Op, Path, NewVal),
    %% if it's a move operator let's delete the element under the given path using
    %% a separate remove operator
    case Op of
        move ->
            #{from := From} = Diff,
            apply_op(NewData, remove, From, undefined);
        _ ->
            NewData
    end.

-spec apply_update_fun(Fun, Operator, NewValue, Data, Path) -> Res when
      Fun :: undefined | fun(),
      Operator :: diff_operator(),
      NewValue :: term(),
      Data :: term(),
      Path :: path(),
      Res :: term().
apply_update_fun(undefined, _Operator, NewValue, _Data, _Path) ->
    NewValue;
apply_update_fun(Fun, Operator, NewValue, Data, Path) ->
    %% first only check if the given operator can be handled with the fun
    case catch Fun(Operator, 0) of
        error ->
            NewValue;
        _ ->
            OldValue = get_val(Data, Path),
            {ok, Result} = Fun(Operator, OldValue),
            Result
    end.

-spec get_new_val(Diff, Data) -> Res when
      Diff :: diff_operator(),
      Data :: term(),
      Res :: term().
get_new_val(#{op := move, from := From}, Data) ->
    get_val(Data, From);
get_new_val(Diff, _Data) ->
    maps:get(value, Diff, undefined).

-spec apply_op(Data, Op, Path, NewValue) -> Res when
      Data :: term(),
      Op :: atom(),
      Path :: path(),
      NewValue :: term(),
      Res :: term().
apply_op(_Data, _Op, [], NewVal) ->
    NewVal;
apply_op(Data, remove, [{Idx, list}], _NewVal) when is_list(Data) ->
    rm_list_el(remove, Data, Idx);
apply_op(Data, remove, [{Key, map}], _NewVal) when is_map(Data) ->
    maps:remove(Key, Data);
apply_op(Data, Op, [{Idx, list} | T], NewVal) when is_list(Data) ->
    ListEl = nth_el(Data, Idx, NewVal),
    RemainingEls = rm_list_el(Op, Data, Idx),
    NewEl = apply_op(ListEl, Op, T, NewVal),
    add_list_el(RemainingEls, Idx, NewEl);
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

-spec add_list_el(List, Idx, El) -> Res when
      List :: list(),
      Idx :: index(),
      El :: term(),
      Res :: list().
add_list_el(List, '-', El) ->
    List ++ [El];
add_list_el(List, Ind0, El) ->
    Ind = Ind0 + 1,
    FirstEls = string:substr(List, 1, Ind - 1),
    LastEls = string:substr(List, Ind),
    FirstEls ++ [El] ++ LastEls.

-spec rm_list_el(Op, List, Idx) -> Res when
      Op :: atom(),
      List :: list(),
      Idx :: index(),
      Res :: list().
rm_list_el(remove, List, '-') ->
    %% remove last element of the list
    string:substr(List, 1, length(List) -1);
rm_list_el(_Op, List, '-') ->
    List;
rm_list_el(_Op, List, Ind0) ->
    Ind = Ind0 + 1,
    [E ||
     {E, I} <- lists:zip(List, lists:seq(1, length(List))), I =/= Ind].

-spec nth_el(List, Idx, NewVal) -> Res when
      List :: list(),
      Idx :: index(),
      NewVal :: term(),
      Res :: term().
nth_el(_List, '-', NewVal) ->
    NewVal;
nth_el(List, Ind0, _NewVal) ->
    Ind = Ind0 + 1,
    lists:nth(Ind, List).

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

maybe_moved_(K, V, [], Path, Log, Acc) ->
    {[#{op => remove, path => path([{K, map} | Path]), orig_value => V} | Log],
     Acc};
maybe_moved_(K, V, [{NewK, V}|Rest], Path, Log, Acc) ->
    {[#{op => move, path => path([{NewK, map} | Path]),
        from => path([{K, map} | Path])} | Log],
     Acc ++ Rest};
maybe_moved_(K, V, [Other|Rest], Path, Log, Acc) ->
    maybe_moved_(K, V, Rest, Path, Log, [Other|Acc]).

-spec list_diff(From, To, Path, Log, Counter) -> Res when
      From :: list(),
      To :: list(),
      Path :: path(),
      Log :: list(),
      Counter :: index(),
      Res :: list(diff_operator()).
list_diff([From|RestF], [To|RestT], Path, Log, Cnt) ->
    list_diff(RestF, RestT, Path, diff(From, To, [{Cnt, list} | Path], Log),
              Cnt+1);
list_diff([V | Rest], [], Path, Log, Cnt) ->
    NewLog = [#{op => remove, path => path([{Cnt, list} | Path]),
                orig_value => V} | Log],
    list_diff(Rest, [], Path, NewLog, Cnt+1);
list_diff([], Rest, Path, Log, _Cnt) ->
  lists:foldl(fun(V, L) ->
              [#{op => add, path => path([{'-', list} | Path]), value => V} | L]
      end, Log, Rest).

list_diff([From|RestF], [To|RestT], Path, Log, Cnt, Fun) ->
    list_diff(RestF, RestT, Path, diff(From, To, [{Cnt, list} | Path], Log, Fun),
              Cnt + 1, Fun);
list_diff([V | Rest], [], Path, Log, Cnt, Fun) ->
    NewLog = [#{op => remove, path => path([{Cnt, list} | Path]),
                orig_value => V} | Log],
    list_diff(Rest, [], Path, NewLog, Cnt + 1, Fun);
list_diff([], Rest, Path, Log, _Cnt, _Fun) ->
    lists:foldl(fun(V, L) ->
        [#{op => add, path => path([{'-', list} | Path]), value => V} | L]
    end, Log, Rest).

-spec path(Path) -> Res when
      Path :: path(),
      Res :: path().
path(Path) ->
    lists:reverse(Path).
