%%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ts=4 sw=4 et
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Proper tests of maps_utils module
%%% @end
%%%-----------------------------------------------------------------------------

-module(prop_maps_utils_test).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("maps_utils_test.hrl").

%%==============================================================================
%% Properties
%%==============================================================================

%%------------------------------------------------------------------------------
%% prop_diff_and_apply_diff
%%------------------------------------------------------------------------------
prop_diff_and_apply_diff(doc) ->
    "Test whether  Diff = diff(D1, D2), D2 == apply_diff(D1, Diff) is always true";
prop_diff_and_apply_diff(opts) ->
    [{numtests, 500}].

prop_diff_and_apply_diff() ->
    ?FORALL({D1, D2}, {map_like_data(), map_like_data()},
        begin
            Diff = maps_utils:diff(D1, D2),
            Actual = maps_utils:apply_diff(D1, Diff),
            ?WHENFAIL(?ERROR("Failing test:\n"
                             "D1 = ~p,\n"
                             "D2 = ~p,\n"
                             "Diff = ~p\n"
                             "Actual value: ~p",
                             [D1, D2, Diff, Actual]),
                      D2 =:= Actual)
        end).

%%------------------------------------------------------------------------------
%% prop_counters
%%------------------------------------------------------------------------------
prop_counters(doc) ->
    "Test funs passed to diff/3 and apply_diff/3. "
    "No replace operators should be present in the diff. The original data "
    "structure should be restored using the old data and the operators "
    "containing counter updates";
prop_counters(opts) ->
    [{numtests, 500}].

prop_counters() ->
    CounterFun =  fun(From, To, Path, Log) ->
                          [#{op => incr, path => Path, value => To - From} | Log]
                  end,
    ReverseFun =
        fun(#{op := incr, value := Value}, OldValue) ->
                {ok, OldValue + Value};
           (_, _) ->
                error
        end,
    ?FORALL({D1, D2}, {map_like_data(), map_like_data()},
        begin
            Diff = maps_utils:diff(D1, D2, CounterFun),
            Actual = maps_utils:apply_diff(D1, Diff, ReverseFun),
            ?WHENFAIL(?ERROR("Failing test:\n"
                             "D1 = ~p,\n"
                             "D2 = ~p,\n"
                             "Diff = ~p\n"
                             "Actual value: ~p",
                             [D1, D2, Diff, Actual]),
                      D2 =:= Actual)
        end).

%%------------------------------------------------------------------------------
%% prop_counters
%%------------------------------------------------------------------------------
prop_revert_diff(doc) ->
    "Test maps_utils:revert_diff/2. Diff = diff(Old, New), "
    "?assertEqual(Old, revert_diff(New, Diff)) should be always true.";
prop_revert_diff(opts) ->
    [{numtests, 500}].

prop_revert_diff() ->
    ?FORALL({D1, D2}, {map_like_data(), map_like_data()},
        begin
            Diff = maps_utils:diff(D1, D2),
            Actual = maps_utils:revert_diff(D2, Diff),
            ?WHENFAIL(?ERROR("Failing test:\n"
                             "D1 = ~p,\n"
                             "D2 = ~p,\n"
                             "Diff = ~p\n"
                             "Actual value: ~p",
                             [D1, D2, Diff, Actual]),
                      D1 =:= Actual)
        end).

%%==============================================================================
%% Generators
%%==============================================================================

-spec map_like_data() -> proplists:proplist().
map_like_data() ->
    ?LET(List, ?SIZED(Size, map_like_data(Size div 5)),
         unique_keys(List, [], [], 1)).

%%==============================================================================
%% Helpers
%%==============================================================================
boolean(_) -> true.

map_like_data(0) ->
    [];
map_like_data(Size) ->
    list(
      frequency([
                 {9, {key(), oneof([1, 2, 3])}},
                 {4, {key(), list(integer())}},
                 {1, {key(), map_like_data(Size - 1)}}
                ])).

unique_keys([], Acc, _Path, _Index) ->
    maybe_to_map(lists:reverse(Acc));
unique_keys([{_Key, Value} | T], Acc, Path, Index) ->
    NewVal = case is_proplist(Value) of
                 true ->
                     unique_keys(Value, [], [Index | Path], 1);
                 false ->
                     Value
             end,
    unique_keys(T, [{unique_key(Path, Index), NewVal} | Acc], Path, Index + 1).

unique_key(Path, Index) ->
    PostFix = string:join(
                [integer_to_list(El) || El <- lists:reverse([Index | Path])], "_"),
    list_to_binary("key_" ++ PostFix).

key() ->
    oneof([key1, key2, key3, key4, key5, key6]).

-spec is_proplist(Val) -> Res when
      Val :: term(),
      Res :: boolean().
is_proplist(Val) ->
    case (catch dict:from_list(Val)) of
        {'EXIT', _} ->
            false;
        _ ->
            true
    end.

maybe_to_map(Proplist) ->
    case rand:uniform(2) of
        1 ->
            maps:from_list(Proplist);
        _ ->
            Proplist
    end.

-spec keys_once(List) -> Res when
      List :: proplists:proplist(),
      Res :: boolean().
keys_once([]) ->
    true;
keys_once([{Key, _Val} | T]) ->
   not proplists:is_defined(Key, T) andalso keys_once(T).
