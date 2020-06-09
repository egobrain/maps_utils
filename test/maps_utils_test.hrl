%% -*- erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% ANSI ESC color codes
%%------------------------------------------------------------------------------
-define(CLR_RED,     "\e[0;31m").
-define(CLR_CYAN,    "\e[0;36m").
-define(CLR_RESET,   "\e[0;0m").

-define(DBG(Fmt, Args),
        io:format(user, ?CLR_CYAN ++ Fmt ++ ?CLR_RESET ++ "\n", Args)).

%%
%% Test will immediately fail if test was called with
%% ABORT_ON_ERROR=true rebar3 test
%%
%% Without the ABORT_ON_ERROR parameter passed to the test error message will
%% be printed with red but test will continue.
%%
-define(ERROR(Fmt, Args),
        begin
            io:format(user, ?CLR_RED ++ Fmt ++ ?CLR_RESET ++ "\n", Args),
            case os:getenv("ABORT_ON_ERROR") of
                "true" ->
                    erlang:halt(1);
                _ ->
                    ok
            end
        end).
