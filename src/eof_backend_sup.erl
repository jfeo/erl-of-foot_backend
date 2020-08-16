%%%-------------------------------------------------------------------
%% @doc eof_backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eof_backend_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {port, Port}).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init({port, Port}) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => eof_ws_server,
                    start => {eof_ws_server, start_link, [{port, Port}]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => dynamic}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
