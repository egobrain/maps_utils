%%%-------------------------------------------------------------------
%%% @doc
%%% Map query language - DSL to modify maps
%%% @end
%%%-------------------------------------------------------------------
-module(mql).
-compile({no_auto_import, [apply/2]}).

-export([apply/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(OTP_RELEASE).
-compile({inline,[take/2]}).
take(Key, Map) ->
    maps:take(Key, Map).

-else.

take(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            {Value, maps:remove(Key, Map)};
        error ->
            error
    end.
-endif.


-type key() :: term().
-type value() :: term().
-type action() :: {set, key(), value()}
                | {append, key(), value()}
                | {remove, key()}
                | {ifeq, key(), value(), Then :: [action()], Else :: [action()]}
                | {ifeq, key(), value(), Then :: [action()]}
                | {ifset, key(), Then :: [action()], Else :: [action()]}
                | {ifset, key(), Then :: [action()]}
                | {ifdef, key(), Then :: [action()], Else :: [action()]}
                | {ifdef, key(), Then :: [action()]}
                | {rename, [{From :: key(), To :: key()}]}
                | {rename, From :: key(), To :: key()}
                | {with, key() | [key()]}
                | {without, key() | [key()]}
                | {replace_all, From :: value(), To :: value()}
                | {remove_all, value()}.
%% @doc
%% Apply actions to modify map.
%% @end
-spec apply([action()], map()) -> map().
apply([], Map) ->
    Map;

%% append Value to list from Map:Key
apply([{append, Key, Value} | Rest], Map) ->
    apply(Rest, Map#{Key => [Value | maps:get(Key, Map, [])]});

%% perform Map#{Key => Value}
apply([{set, Key, Value} | Rest], Map) ->
    apply(Rest, Map#{Key => Value});

%% remove Key from map
apply([{remove, Key} | Rest], Map) ->
    apply(Rest, maps:remove(Key, Map));

%% apply actions list Apply only if Key exists in Map and equal to Val
%% else apply ApplyElse actions list if presented
apply([{ifeq, Key, Val, Apply} | Rest], Map) ->
    apply([{ifeq, Key, Val, Apply, []} | Rest], Map);
apply([{ifeq, Key, Val, Apply, ApplyElse} | Rest], Map) ->
    case maps:find(Key, Map) of
        {ok, Val} -> apply(Apply ++ Rest, Map);
        _         -> apply(ApplyElse ++ Rest, Map)
    end;

%% apply actions list Apply only if Key exists in Map and not 'undefined' or 'null'
%% else apply ApplyElse actions list if presented
apply([{ifset, Key, Apply} | Rest], Map) ->
    apply([{ifset, Key, Apply, []} | Rest], Map);
apply([{ifset, Key, Apply, ApplyElse} | Rest], Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> apply(ApplyElse ++ Rest, Map);
        null      -> apply(ApplyElse ++ Rest, Map);
        _-> apply(Apply ++ Rest, Map)
    end;

%% apply actions list Apply only if Key exists in Map
%% else apply ApplyElse actions list if presented
apply([{ifdef, Key, Apply} | Rest], Map) ->
    apply([{ifdef, Key, Apply, []} | Rest], Map);
apply([{ifdef, Key, Apply, ApplyElse} | Rest], Map) ->
    case maps:is_key(Key, Map) of
        true -> apply(Apply ++ Rest, Map);
        false-> apply(ApplyElse ++ Rest, Map)
    end;

%% rename key From to key To
apply([{rename, FromToList} | Rest], Map) ->
    apply([{rename, F,T} || {F,T} <- FromToList] ++ Rest, Map);
apply([{rename, From, To} | Rest], Map) ->
    case take(From, Map) of
        {Value, Map2} -> apply(Rest, Map2#{To => Value});
        _ -> apply(Rest, Map)
    end;

%% remove all keys from map, who not presented in list Keys
apply([{with, Keys} | Rest], Map) when is_list(Keys) ->
    apply(Rest, maps:with(Keys, Map));
apply([{with, Key} | Rest], Map) ->
    apply(Rest, maps:with([Key], Map));

%% remove Keys from map
apply([{without, Keys} | Rest], Map) when is_list(Keys) ->
    apply(Rest, maps:without(Keys, Map));
apply([{without, Key} | Rest], Map) ->
    apply(Rest, maps:without([Key], Map));

%% replace value of all entries of From to To
%% for example: {replace_all, undefined, null}
apply([{replace_all, From, To} | Rest], Map) ->
    apply(Rest, maps:map(fun(_,V) when V == From -> To;
                                   (_,V)-> V end, Map));

%% remove all records where value equal to Value
apply([{remove_all, Value} | Rest], Map) ->
    apply(Rest, maps:filter(fun(_,V)-> V /= Value end, Map)).



%% --------------- Tests ----------------------------------
-ifdef(TEST).
complex_test() ->
    Map = #{path => "localhost",
            body => <<>>,
            headers => [],
            code => 400,
            etag => undefined,
            user_key => ok},
    Modificators = [{ifdef, path,
                     [{set, version, 1}],
                     [{set, version, 2}]},
                    {ifeq, code, 200,
                     [{set, status, "OK"}],
                     [{without, body},
                      {ifeq, code, 400,
                       [{set, status, "malformed request"}],
                       [{set, status, "unexpected error"}]}]},
                    {ifset, body,
                     [{append, headers, {"Content-Encoding", "gzip"}},
                      {append, headers, {"Content-Type", "application/json"}}]},
                    {with, [path, body, headers, code, version, status, etag]},
                    {replace_all, undefined, null}
                   ],
    TestMap1 = apply(Modificators, Map),
    ?assertMatch(#{status := "malformed request",
                   version := 1,
                   etag := null,
                   headers := []}, TestMap1),
    ?assertNot(maps:is_key(body, TestMap1)),
    ?assertNot(maps:is_key(user_key, TestMap1)),

    TestMap2 = apply([{without, path} | Modificators], Map#{code => 200}),
    ?assertMatch(#{status := "OK",
                   version := 2,
                   body := <<>>,
                   headers := [{"Content-Type", "application/json"},
                               {"Content-Encoding", "gzip"}]}, TestMap2).
-endif.
