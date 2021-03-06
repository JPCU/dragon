%% @private
-module(eradius_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ------------------------------------------------------------------------------------------
%% -- supervisor callbacks
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    DictServer   = {dict, {eradius_dict, start_link, []}, permanent, brutal_kill, worker, [eradius_dict]},
    RadiusLog    = {radius_log, {eradius_log, start_link, []}, permanent, brutal_kill, worker, [eradius_log]},
    ServerTopSup = {server_top_sup, {eradius_server_top_sup, start_link, []}, permanent, infinity, supervisor, [eradius_server_top_sup]},
   % Snarl        = {libsnarl_sup, {libsnarl_sup, start_link, []}, permanent, infinity, supervisor, [libsnarl_sup]},
 %   Client       = {client, {eradius_client, start_link, []}, permanent, 500, worker, [eradius_client]},

    {ok, {SupFlags, [DictServer, RadiusLog, ServerTopSup]}}.
