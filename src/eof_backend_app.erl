%%%-------------------------------------------------------------------
%% @doc eof_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(eof_backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    eof_backend_sup:start_link(5000).

stop(_State) ->
    ok.

%% internal functions
