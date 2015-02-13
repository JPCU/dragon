-module(eradius_config).
% Eradius API's:
-export([create_server_config/1]).
% Config validating API functions:
-export([validate_ip/1, validate_port/1, validate_ports/1,
         map_helper/3, map_helper/2, ok_error_helper/2, validate_secret/1]).

-include_lib("../include/eradius_lib.hrl").

%% ------------------------------------------------------------------------------------------
%% -- config validation
-define(pos_int(X), is_integer(X), X >= 0).
-define(ip4_address_num(X), ?pos_int(X), X < 256).
-define(ip4_address(T), ?ip4_address_num(element(1, T)), ?ip4_address_num(element(2, T)),
                        ?ip4_address_num(element(3, T)), ?ip4_address_num(element(4, T))).
-define(valid_atom(Value), Value =/= invalid).
-define(valid(X), is_tuple(X), ?valid_atom(element(1, X))).
-define(is_io(IO), is_list(IO) orelse is_binary(IO)).
-define(invalid(ErrorMsg, ErrorValue), {invalid, io_lib:format(ErrorMsg, ErrorValue)}).



create_server_config(HandlerModule) ->
    case server_config(#server_config{}) of
        {ok, S} ->
            case application:get_env(nas_list) of
                {ok, ListOfNases} ->
                    case validate_naslist(HandlerModule, ListOfNases) of
                        {ok, NASList} ->
                            {ok, {{S#server_config.snarl_realm, S#server_config.ip_address, S#server_config.port}, NASList}};
                        _ ->
                            {error, <<"Invalid NAS list">>}
                    end;
                _ ->
                    {error, <<"NAS list not specified">>}
            end;
        _ ->
            {error, <<"Bad Server Config">>}
    end.
    



server_config(S = #server_config{snarl_realm = <<>>}) ->
    case application:get_env(snarl_realm) of
        {ok, SnarlRealm} ->
            server_config(S#server_config{snarl_realm=SnarlRealm});
        _ ->
            lager:error("eradius_config could not load application env varible: snarl_realm~n"),
            error
    end;

server_config(S = #server_config{ip_address = {} }) ->
    case application:get_env(ip_address) of
        {ok, IPAddress} ->
            server_config(S#server_config{ip_address=IPAddress});
        _ ->
            lager:error("eradius_config could not load application env varible: ip_address~n"),
            error
    end;

server_config(S = #server_config{port=0}) ->
    case application:get_env(port) of
        {ok, Port} ->
            server_config(S#server_config{port=Port});
        _ ->
            lager:error("eradius_config could not load application env varible: port~n"),
            error
    end;

server_config(S) ->
    {ok, S}.




validate_naslist(HandlerModule, ListOfNases) -> 
    validate_naslist(HandlerModule, ListOfNases, []).

validate_naslist(_HandlerModule, [{_NasID, _NasIP, _Secret, []}| _T], _Collector) ->
    {error, <<"Invalid NAS Permissions">>};
validate_naslist(HandlerModule, [{NasID, NasIP, Secret, Permissions}| T], Collector) ->
    case validate_ip(NasIP) of
        ok ->
            validate_naslist(HandlerModule, T, [{NasID, NasIP, Secret, HandlerModule, Permissions}| Collector]);
        _ ->
            {error, <<"Invalid NAS IP address">>}
    end;
validate_naslist(_, [], Collector) -> 
    {ok, lists:flatten(Collector)};
validate_naslist(_, _, _) ->
    {error, <<"Bad NAS Config">>}.



% --------------------------------------------------------------------------------------------------
% -- direct validation function

validate_ip(IP) when is_list(IP) ->
    ok_error_helper(inet_parse:address(IP), {"bad IP address: ~p", [IP]});
validate_ip(IP) when ?ip4_address(IP) ->
    ok;
validate_ip(X) ->
    ?invalid("bad IP address: ~p", [X]).

validate_ports(Ports) -> map_helper(fun validate_port/1, Ports).
validate_port(Port) when is_list(Port) -> validate_port(catch list_to_integer(Port));
validate_port(Port) when ?pos_int(Port) -> Port;
validate_port(Port) when is_integer(Port) -> ?invalid("port number out of range: ~p", [Port]);
validate_port(Port) -> ?invalid("bad port number: ~p", [Port]).


% --------------------------------------------------------------------------------------------------
% -- build right format function

validate_secret(Secret) when is_list(Secret) ->
    unicode:characters_to_binary(Secret);
validate_secret(Secret) when is_binary(Secret) ->
    Secret;
validate_secret(OtherTerm) ->
    {invalid, io_lib:format("bad RADIUS secret: ~p", [OtherTerm])}.



% --------------------------------------------------------------------------------------------------
% -- helpers


map_helper(Fun, Values) ->
    map_helper(Fun, Values, no).
map_helper(Fun, Values, Type) ->
    map_helper(Fun, Values, Type, []).
map_helper(_Fun, [], _Type, Values) -> lists:reverse(Values);
map_helper(Fun, [Head | Tail], Type, Values) ->
    case Fun(Head) of
        {invalid, _} = Error ->
            Error;
        Result when Type =/= no andalso is_list(Result) ->
            map_helper(Fun, Tail, Type, Result ++ Values);
        Result ->
            map_helper(Fun, Tail, Type, [Result | Values])
    end.

ok_error_helper({error, _Error}, {Msg, Value}) when is_list(Msg) -> ?invalid(Msg, Value);
ok_error_helper({error, _Error}, ErrorMsg) when is_list(ErrorMsg) -> ErrorMsg;
ok_error_helper({ok, Value}, _ErrorMessage) -> Value;
ok_error_helper(Value, _ErrorMessage) -> Value.
