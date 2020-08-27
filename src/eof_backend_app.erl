%%%-------------------------------------------------------------------
%% @doc eof_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(eof_backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, Port) ->
    eof_backend_sup:start_link(Port).

stop(_State) ->
    ok.

%% internal functions
