%%%-------------------------------------------------------------------
%% @doc eof_backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eof_backend_sup).

-behaviour(supervisor).

-export([start_link/1, start_socket/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {port, Port}).

start_socket() ->
    try supervisor:start_child(?MODULE, []) of
        Result -> Result
    catch
        Error -> Error
    end.

init({port, Port}) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    spawn_link(fun empty_listeners/0),
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 60,
                 period => 3600},
    ChildSpecs = [#{id => eof_ws_server,
                    start => {eof_ws_server, start_link, [ListenSocket]},
                    restart => temporary,
                    shutdown => 1000,
                    type => worker,
                    modules => [eof_ws_server]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.
