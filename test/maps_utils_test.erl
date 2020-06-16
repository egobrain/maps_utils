%% -*- erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%------------------------------------------------------------------------------

-module(maps_utils_test).

-include("maps_utils_test.hrl").
-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Test cases
%%==============================================================================

rename_key_test() ->
    ?assertEqual(#{b => 1}, maps_utils:rename_key(a, b, #{a => 1})).

rename_keys_test() ->
    ?assertEqual(
        #{"a" => 1, "b" => 2},
        maps_utils:rename_keys(fun erlang:atom_to_list/1, #{a => 1, b => 2})).

map_and_rename_test() ->
    ?assertEqual(
        #{"a" => 2, "b" => 4},
        maps_utils:map_and_rename(
            fun(K, V) -> {atom_to_list(K), V * 2} end,
            #{a => 1, b => 2})).

filter_test() ->
    ?assertEqual(
         #{a => 1, b => 2},
         maps_utils:filter(
           fun(_, V) -> V < 3 end,
           #{a => 1, b => 2, c => 3, d => 4})).

filtermap_test_() ->
    [?_test(?assertEqual(#{},
                         maps_utils:filtermap(fun(_K, V) -> {true, V} end, #{}))),
     ?_test(?assertEqual(#{},
                         maps_utils:filtermap(fun(_K, _V) -> false end,
                                              #{a => 1, b => 2}))),
     ?_test(?assertEqual(
         #{a => 2, c => 6},
         maps_utils:filtermap(fun(_K, V) ->
             case V rem 2 of
                 1 -> {true, V*2};
                 _ -> false
             end
         end, #{a => 1, b => 2, c => 3})))].

diff_test_() ->
    [?_test(?assertEqual([], maps_utils:diff(#{a => 1}, #{a => 1}))),
     ?_test(?assertEqual(
         [#{op => replace, orig_value => 1, path => [{a, map}], value => 2}],
         maps_utils:diff(#{a => 1}, #{a => 2}))),
     ?_test(?assertEqual(
         [#{op => move, path => [{b, map}], from => [{a, map}]}],
         maps_utils:diff(#{a => 1}, #{b => 1}))),
     ?_test(?assertEqual(
         [#{op => add, path => [{a, map}], value => 1}],
         maps_utils:diff(#{}, #{a => 1}))),
     ?_test(?assertEqual(
         [#{op => remove, orig_value => 1, path => [{a, map}]}],
         maps_utils:diff(#{a => 1}, #{}))),
     ?_test(?assertEqual(
         [#{op => remove, orig_value => 1, path => [{a, map}]},
          #{op => add, path => [{c, map}], value => 3},
          #{op => add, path => [{b, map}], value => 2}],
         maps_utils:diff(#{a => 1}, #{b => 2, c => 3}))),
     ?_test(?assertEqual(
         [#{op => move, path => [{e, map}], from => [{a, map}]},
          #{op => replace, orig_value => 1,
            path => [{b, map}, {0, list}], value => 2},
          #{op => replace, orig_value => 3,
            path => [{b, map}, {1, list}, {c, map}], value => 4},
          #{op => add, path => [{b, map}, {'-', list}], value => 7},
          #{op => add, path => [{k, map}], value => #{l => 1}}
         ],
         maps_utils:diff(#{a => 1, b => [1, #{c => 3}], d => 4},
                         #{e => 1,
                           b => [2, #{c => 4}, 7], d => 4, k => #{l => 1}}))),
     ?_test(?assertEqual(
         [
          #{op => move, path => [{e, map}], from => [{a, map}]},
          #{op => replace, orig_value => 1,
            path => [{b, map}, {0, list}], value => 2},
          #{op => replace, orig_value => 3,
            path => [{b, map}, {1, list}, {c, map}], value => 4},
          #{op => remove, orig_value => 7, path => [{b, map}, {2, list}]},
          #{op => add, path => [{k, map}], value => #{l => 1}}
         ],
         maps_utils:diff(#{a => 1, b => [1, #{c => 3}, 7], d => 4},
                         #{e => 1, b => [2, #{c => 4}], d => 4, k => #{l => 1}})))].

merge_with_test_() ->
    [?_test(?assertEqual(#{a => 1}, maps_utils:merge_with(fun(A, B) -> A+B end,
                                                          #{}, #{a => 1}))),
     ?_test(?assertEqual(#{a => 1}, maps_utils:merge_with(fun(A, B) -> A+B end,
                                                          #{a => 1}, #{}))),
     ?_test(?assertEqual(#{a => 1, b => 1}, maps_utils:merge_with(
                                                fun(A, B) -> A+B
                                              end, #{a => 1}, #{b => 1}))),
     ?_test(?assertEqual(#{a => 3}, maps_utils:merge_with(
                                      fun(A, B) -> A+B end,
                                      #{a => 1}, #{a => 2})))].

update_test_() ->
    [?_test(?assertEqual(#{a => 1}, maps_utils:update(a, #{}, 1,
                                                      fun(A) -> A+2 end))),
     ?_test(?assertEqual(#{a => 5}, maps_utils:update(a, #{a => 3}, 1,
                                                      fun(A) -> A+2 end)))].

apply_diff_test() ->
    chk_apply(#{k1 => 1, k2 => #{k21 => 21, k22 => 22}},
                #{k1 => 1, k2 => #{k21 => 21, k22 => 23}}),
    chk_apply(#{k1 => 1, k2 => 2},
              #{k1 => 1, k3 => 2}),
    chk_apply(#{k1 => #{k11 => 11, k12 => 12}},
              #{k1 => #{k11 => 11, k13 => 12}}),
    chk_apply([#{k1 => 1, k2 => 2}, #{k3 => 3, k4 => 4}],
              [#{k1 => 1, k2 => 3}, #{k4 => 2}]),
    chk_apply(#{k1 => [1, 2, 3]},
              #{k1 => [1, 2, 3, 4]}),
    chk_apply(#{k1 => [1, 2, 3]},
              #{k1 => [1, 2]}).

get_val_test() ->
    ?assertEqual(1, maps_utils:get_val([[1, 2], [3, 4]],
                                       [{0, list}, {0, list}])).

sort_operators_test() ->
    ?assertEqual(
         [
          #{op => add, path => [{1, list}]},
          #{op => remove, path => [{0, list}, {2, list}]},
          #{op => remove, path => [{0, list}, {1, list}]},
          #{op => add, path => [{2, list}]},
          #{op => remove, path => [{0, list}, {0, list}]}
         ],
       maps_utils:sort_remove_operators(
         [
          #{op => add, path => [{1, list}]},
          #{op => remove, path => [{0, list}, {0, list}]},
          #{op => remove, path => [{0, list}, {2, list}]},
          #{op => add, path => [{2, list}]},
          #{op => remove, path => [{0, list}, {1, list}]}
         ])).

diff_nums_test_() ->
    Fun = fun(_, To, _, Log) -> [#{fun_result => To} | Log] end,
    [
     ?_test(?assertEqual([], maps_utils:diff(#{a => 1}, #{a => 1}, Fun))),
     ?_test(?assertEqual([#{fun_result => 2}],
                         maps_utils:diff(#{a => 1}, #{a => 2}, Fun))),
     ?_test(?assertEqual(
               [
                #{ op => move, path => [{b, map}], from => [{a, map}]}
               ],
               maps_utils:diff(#{a => 1}, #{b => 1}, Fun))),
     ?_test(?assertEqual(
               [
                #{ op => add, path => [{a, map}], value => 1}
               ],
               maps_utils:diff(#{}, #{a => 1}, Fun))),
     ?_test(?assertEqual(
               [
                #{ op => remove, orig_value => 1, path => [{a, map}]}
               ],
               maps_utils:diff(#{a => 1}, #{}, Fun))),
     ?_test(?assertEqual(
               [
                #{op => remove, orig_value => 1, path => [{a, map}]},
                #{op => add, path =>  [{c, map}], value => 3},
                #{op => add, path => [{b, map}], value => 2}
               ],
               maps_utils:diff(#{a => 1}, #{b => 2, c => 3}, Fun))),
     ?_test(?assertEqual(
               [#{from => [{a, map}], op => move, path => [{e, map}]},
                #{fun_result => 2},
                #{fun_result => 4},
                #{op => add, path => [{b, map}, {'-', list}], value => 7},
                #{op => add, path => [{k, map}], value => #{l => 1}}],
               maps_utils:diff(#{a => 1, b => [1, #{c => 3}], d => 4},
                               #{e => 1, b => [2, #{c => 4}, 7], d => 4,
                                 k => #{l => 1}},
                               Fun))),
     ?_test(?assertEqual(
               [#{fun_result => 2},
                #{op => replace, orig_value => x, path => [{b, map}], value => y},
                #{fun_result => 3},
                #{fun_result => <<"3">>},
                #{op => remove, orig_value => 2, path => [{e, map}, {1, list}]}],
               maps_utils:diff(#{a => 1, b => x, c => 4, d => <<"4">>,
                                 e => [1, 2]},
                               #{a => 2, b => y, c => 3, d => <<"3">>,
                                 e => [1]}, Fun)))
    ].

apply_fun_test() ->
    ReverseCounterFun =
        fun(#{op := incr, value := Value}, OldValue) ->
                {ok, OldValue + Value};
           (_, _) ->
                error
        end,
    ?assertEqual(
           #{key1 => 4, key2 => 2},
           maps_utils:apply_diff(
             #{key1 => 1},
             [#{op => add, path => [{key2, map}], value => 2},
              #{op => incr, path => [{key1, map}], value => 3}],
             ReverseCounterFun)),
    ?assertEqual(
       #{key1 => 4, key2 => #{key21 => 31}},
       maps_utils:apply_diff(
         #{key1 => 1, key2 => #{key21 => 21}},
         [#{op => incr, path => [{key2, map}, {key21, map}],
            value => 10},
          #{op => incr, path => [{key1, map}], value => 3}],
         ReverseCounterFun)).

revert_diff_test() ->
    ?assertEqual(#{op => incr}, maps_utils:revert_op(#{op => incr})),
    chk_revert(#{k1 => 1, k2 => #{k21 => 21, k22 => 22}},
               #{k1 => 1, k2 => #{k21 => 21, k22 => 23}}),
    chk_revert(#{k1 => 1, k2 => 2},
               #{k1 => 1, k3 => 2}),
    chk_revert(#{k1 => #{k11 => 11, k12 => 12}},
               #{k1 => #{k11 => 11, k13 => 12}}),
    chk_revert([#{k1 => 1, k2 => 2}, #{k3 => 3, k4 => 4}],
               [#{k1 => 1, k2 => 3}, #{k4 => 2}]),
    chk_revert(#{k1 => [1, 2, 3]},
               #{k1 => [1, 2, 3, 4]}),
    chk_revert(#{k1 => [1, 2, 3]},
               #{k1 => [1, 2]}).

%%==============================================================================
%% Performance tests
%%==============================================================================

perf_test_() ->
    {timeout, 20, [
                   fun diff_and_apply_diff/0
                  ]}.

diff_and_apply_diff() ->
    D1 = [
          #{id => "0001", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0002", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0003", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0004", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0005", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0006", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0007", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0008", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0009", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0010", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }}],
    D2 = [
          #{id => "0011", data => #{  %% change id 0001 -> 0011
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0002", data => #{
                            name => "ahoy popacsek",
                            addr => "ot azoy str no 999",   %% address -> addr
                            age => 99,
                            number_of_children => 318
                           }},
          #{id => "0003", data => #{
                            name => "mordechai brochi",   %% change name
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0004", data => #{
                            %% delete age field
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            number_of_children => 3
                           }},
          #{id => "0005", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            %% add DOB
                            dob => {1820, 10, 01},
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0006", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0007", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0008", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0009", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }},
          #{id => "0010", data => #{
                            name => "ahoy popacsek",
                            address => "ot azoy str no 999",
                            age => 99,
                            number_of_children => 3
                           }}],
    Diff = maps_utils:diff(D1, D2),
    perf_test(fun() -> maps_utils:diff(D1, D2) end, 15000, 2000,
              "maps_utils:diff/2"),
    perf_test(fun() -> maps_utils:apply_diff(D1, Diff) end, 15000, 2000,
              "maps_utils:apply_diff/2").

convert_diff_paths_test() ->
    ?assertEqual(
       [#{op => replace, value => 2,
          path => [{key1, map}, {key2, map}, {1, list}]},
        #{op => remove,
          path => [{key1, map}, {1, list}, {key2, map}, {2, list}]}],
       maps_utils:convert_diff_paths(
         [#{op => replace, value => 2, path => <<"/key1/key2/1">>},
          #{op => remove, path => <<"/key1/1/key2/2">>}], atom)),
    ?assertEqual(
       [#{op => replace, value => 2,
          path => [{"key1", map}, {"key2", map}, {1, list}]},
        #{op => remove,
          path => [{"key1", map}, {1, list}, {"key2", map}, {2, list}]}],
       maps_utils:convert_diff_paths(
         [#{op => replace, value => 2, path => <<"/key1/key2/1">>},
          #{op => remove, path => <<"/key1/1/key2/2">>}], list)),
    ?assertEqual(
       [#{op => replace, value => 2,
          path => [{<<"key1">>, map}, {<<"key2">>, map}, {1, list}]},
        #{op => remove,
          path => [{<<"key1">>, map}, {1, list}, {<<"key2">>, map}, {2, list}]}],
       maps_utils:convert_diff_paths(
         [#{op => replace, value => 2, path => <<"/key1/key2/1">>},
          #{op => remove, path => <<"/key1/1/key2/2">>}], binary)).


%%==============================================================================
%% Helper functions
%%==============================================================================

chk_apply(D1, D2) ->
    Diff = maps_utils:diff(D1, D2),
    Current = maps_utils:apply_diff(D1, Diff),
    ?assertEqual(D2, Current).

chk_revert(D1, D2) ->
    Diff = maps_utils:diff(D1, D2),
    Current = maps_utils:revert_diff(D2, Diff),
    ?assertEqual(D1, Current).

perf_test(ExecFun, Count, MinExecPerSec, FunctionName) ->
    Lst = lists:seq(1, Count),
    {Time, _} = timer:tc(fun() ->
                            lists:foreach(fun(_) -> ExecFun() end, Lst)
                         end),
    ExecPerSec = Count / (Time / 1000000),
    ?DBG("Performance of ~s: ~p", [FunctionName, ExecPerSec]),
    case ExecPerSec < MinExecPerSec of
        true ->
            ?ERROR("Execution of ~s is too slow. Min should be: ~p/sec."
                   "Current speed: ~p/sec",
                   [FunctionName, MinExecPerSec, ExecPerSec]);
        false ->
            ok
    end.
