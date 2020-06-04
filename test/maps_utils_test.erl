%% -*- erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%------------------------------------------------------------------------------

-module(maps_utils_test).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Helper functions
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
         [#{op => replace, path => [{a, map}], value => 2}],
         maps_utils:diff(#{a => 1}, #{a => 2}))),
     ?_test(?assertEqual(
         [#{op => move, path => [{b, map}], from => [{a, map}]}],
         maps_utils:diff(#{a => 1}, #{b => 1}))),
     ?_test(?assertEqual(
         [#{op => add, path => [{a, map}], value => 1}],
         maps_utils:diff(#{}, #{a => 1}))),
     ?_test(?assertEqual(
         [#{op => remove, path => [{a, map}]}],
         maps_utils:diff(#{a => 1}, #{}))),
     ?_test(?assertEqual(
         [#{op => remove, path => [{a, map}]},
          #{op => add, path => [{c, map}], value => 3},
          #{op => add, path => [{b, map}], value => 2}],
         maps_utils:diff(#{a => 1}, #{b => 2, c => 3}))),
     ?_test(?assertEqual(
         [#{op => move, path => [{e, map}], from => [{a, map}]},
          #{op => replace, path => [{b, map}, {0, list}], value => 2},
          #{op => replace, path => [{b, map}, {1, list}, {c, map}], value => 4},
          #{op => add, path => [{b, map}, {'-', list}], value => 7},
          #{op => add, path => [{k, map}], value => #{l => 1}}
         ],
         maps_utils:diff(#{a => 1, b => [1, #{c => 3}], d => 4},
                         #{e => 1,
                           b => [2, #{c => 4}, 7], d => 4, k => #{l => 1}}))),
     ?_test(?assertEqual(
         [
          #{op => move, path => [{e, map}], from => [{a, map}]},
          #{op => replace, path => [{b, map}, {0, list}], value => 2},
          #{op => replace, path => [{b, map}, {1, list}, {c, map}], value => 4},
          #{op => remove, path => [{b, map}, {2, list}]},
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
    % TODO: fix move
%    maps_utils_test 127 {D1, D2, Diff, Current}:
%    '{#{k1 => 1,k2 => 2},
%       #{k1 => 1,k3 => 2},
%       [#{from => [{k2,map}],op => move,path => [{k3,map}]}],
%       #{k1 => 1,k2 => 2,k3 => undefined}}'
    chk_apply(#{k1 => 1, k2 => 2},
              #{k1 => 1, k3 => 2}),
    chk_apply([#{k1 => 1, k2 => 2}, #{k3 => 3, k4 => 4}],
              [#{k1 => 1, k2 => 3}, #{k4 => 2}]),
    chk_apply(#{k1 => [1, 2, 3]},
              #{k1 => [1, 2, 3, 4]}),
    chk_apply(#{k1 => [1, 2, 3]},
              #{k1 => [1, 2]}).

%%==============================================================================
%% Helper functions
%%==============================================================================

chk_apply(D1, D2) ->
    Diff = maps_utils:diff(D1, D2),
    Current = maps_utils:apply_diff(D1, Diff),
    io:format(user, "~p ~p {D1, D2, Diff, Current}: '~130p' ~n", [?MODULE, ?LINE, {D1, D2, Diff, Current}]),
    ?assertEqual(D2, Current).
