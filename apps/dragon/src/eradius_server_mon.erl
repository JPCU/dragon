%% @private
%% @doc Manager for RADIUS server processes.
%%   This module manages the RADIUS server registry and
%%   validates and applies the server configuration from the application environment.
%%   It starts all servers that are configured as part of its initialization,
%%   then sends ping requests to all nodes that are part of the configuration in order
%%   to keep them connected.
-module(eradius_server_mon).
-export([start_link/0, reconfigure/0, lookup_handler/3, lookup_pid/2, set_trace/4, all_nas_keys/0]).
-export_type([handler/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eradius_lib.hrl").

-define(SERVER, ?MODULE).
-define(NAS_TAB, eradius_nas_tab).
-export_type([server/0]).
-type server()  :: {inet:ip_address(), eradius_server:port_number()}.
-type handler() :: {module(), term()}.

-record(nas, {
    key :: {server(), inet:ip_address()},
    handler :: handler(),
    prop :: #nas_prop{}
}).

%% ------------------------------------------------------------------------------------------
%% -- API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Apply NAS config from the application environment.
%%   Walks the list of configured servers and NASs,
%%   starting and stopping servers as necessary.
%%   If the configuration is invalid, no servers are modified.
%%
-spec reconfigure() -> ok | {error, invalid_config}.
reconfigure() ->
    gen_server:call(?SERVER, reconfigure).

%% @doc Fetch the RADIUS secret, handler and trace flag for a given server/NAS combination.
%%   This is a very fast operation that is called for every
%%   request by the RADIUS server process.
-spec lookup_handler(inet:ip_address(), eradius_server:port_number(), inet:ip_address()) -> {ok, handler(), #nas_prop{}} | {error, not_found}.
lookup_handler(IP, Port, NasIP) ->
    case ets:lookup(?NAS_TAB, {{IP, Port}, NasIP}) of
        [] ->
            {error, not_found};
        [Rec] ->
            Prop = (Rec#nas.prop)#nas_prop{server_ip = IP, server_port = Port},
            {ok, Rec#nas.handler, Prop}
    end.

%% @doc Fetches the pid of RADIUS server at IP:Port, if there is one.
-spec lookup_pid(inet:ip_address(), eradius_server:port_number()) -> {ok, pid()} | {error, not_found}.
lookup_pid(ServerIP, ServerPort) ->
    gen_server:call(?SERVER, {lookup_pid, {ServerIP, ServerPort}}).

%% @doc returns the list of all currently configured NASs
-spec all_nas_keys() -> [term()].
all_nas_keys() ->
	ets:select(?NAS_TAB, [{#nas{key = '$1', _ = '_'}, [], ['$1']}]).

%% @doc Set or clear the trace flag for a given Server/NAS combination.
-spec set_trace(inet:ip_address(), eradius_server:port_number(), inet:ip_address(), boolean()) -> ok.
set_trace(ServerIP, ServerPort, NasIP, Trace) when is_boolean(Trace) ->
    gen_server:call(?SERVER, {set_trace, {{ServerIP, ServerPort}, NasIP}, Trace}).

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {running}).

init([]) ->
    ?NAS_TAB = ets:new(?NAS_TAB, [named_table, protected, {keypos, #nas.key}]),
    case configure(#state{running = []}) of
        {error, invalid_config} -> {stop, invalid_config};
        Else                    -> Else
    end.

handle_call({lookup_pid, Server}, _From, State) ->
    case proplists:get_value(Server, State#state.running) of
        undefined ->
            {reply, {error, not_found}, State};
        Pid ->
            {reply, {ok, Pid}, State}
    end;
handle_call({set_trace, NasKey, Trace}, _From, State) ->
    case ets:lookup(?NAS_TAB, NasKey) of
        [] ->
            {reply, {error, not_found}, State};
        [Rec = #nas{prop = Prop}] ->
            NewNas = Rec#nas{prop = Prop#nas_prop{trace = Trace}},
            ets:insert(?NAS_TAB, NewNas),
            {reply, ok, State}
    end;
handle_call(reconfigure, _From, State) ->
    case configure(State) of
        {error, invalid_config} -> {reply, {error, invalid_config}, State};
        {ok, NState}            -> {reply, ok, NState}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% unused callbacks
handle_cast(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- helpers

configure(#state{running = Running}) ->
    case eradius_config:create_server_config(dragon_snarl_handler) of 
        {ok, Config} ->
            {{ServerIpAddress, ServerPort}, NASList} = Config,
            ServList =  [{{ServerIpAddress, ServerPort}, NASList}],
            NasList = lists:flatmap(fun(Server) ->
                List = server_naslist(Server),
                ets:insert(?NAS_TAB, List),
                List
            end, ServList),
            ets:foldl(fun(Nas, _) ->
                case lists:member(Nas, NasList) of
                    true    -> done;
                    false   -> ets:delete(?NAS_TAB, Nas)
                end
            end, [], ?NAS_TAB),
            Run     = sets:from_list([element(1, T) || T <- Running]),
            New     = sets:from_list([element(1, T) || T <- ServList]),
            ToStart = sets:subtract(New, Run),
            ToStop  = sets:subtract(Run, New),
            Started = sets:fold(fun (Key, List) ->
                lists:keydelete(Key, 1, List),
                {{IP, Port}, Pid} = lists:keyfind(Key, 1, Running),
                eradius_server_sup:stop_instance(IP, Port, Pid)
                end, Running, ToStop),
            NRunning = sets:fold(fun ({IP, Port}, Acc) ->
                case eradius_server_sup:start_instance(IP, Port) of
                    {ok, Pid} ->
                        [{{IP, Port}, Pid} | Acc];
                    {error, Error} ->
                        Host = inet_parse:ntoa(IP),
                        dragon:error_report("Could not start listener ~s:~b: ~p~n", [Host, Port, Error]),
                        Acc
                end
            end, Started, ToStart),
            {ok, #state{running = NRunning}};
        {error, Message} ->
            dragon:error_report("invalid server config: ~s", [Message]),
            {error, invalid_config}
    end.

%-spec server_naslist(valid_server()) -> list(#nas{}).
server_naslist({{IP, Port}, HandlerList}) ->
    [#nas{key = {{IP, Port}, NasIP},
          handler = {HandlerMod, HandlerArgs},
          prop = #nas_prop{nas_id = NasId, nas_ip = NasIP, secret = Secret, trace = false}}
      || {NasId, NasIP, Secret, HandlerMod, HandlerArgs} <- HandlerList].

