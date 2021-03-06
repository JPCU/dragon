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
            R = create_naslist(HandlerModule),
            %case application:get_env(nas_list) of
            %    {ok, ListOfNases} ->
            %        case validate_naslist(HandlerModule, ListOfNases) of
            %            {ok, NASList} ->
                            {ok, {{S#server_config.ip_address, S#server_config.port}, R}};
            %            _ ->
            %                {error, <<"Invalid NAS list">>}
            %        end;
            %    _ ->
            %        {error, <<"NAS list not specified">>}
            %end;
        _ ->
            {error, <<"Bad Server Config">>}
    end.
    


server_config(S = #server_config{ip_address = {} }) ->
    case application:get_env(listener_address) of
        {ok, {IPAddress, Port}} ->
            {ok, IP} = inet_parse:address(IPAddress),
            server_config(S#server_config{ip_address=IP, port=Port});
        _ ->
            lager:error("eradius_config could not load application env varible: ip_address~n"),
            error
    end;


server_config(S) ->
    {ok, S}.


create_naslist(HandlerModule) ->
    {ok, FullList} = application:get_env('NAS'),
    AddressList = create_address_list(FullList, []),
    SecretsList = create_secret_list(FullList, []),
    PermissionsList = create_permissions_list(FullList, []),
    MFAList = create_mfa_list(FullList, []),
    compile_naslist(HandlerModule, AddressList, SecretsList, PermissionsList, MFAList, []).

compile_naslist(HandlerModule, [{Name, IP} | T], SecretsList, PermissionsList, MFAList, CompiledList) ->
    Secret = get_value_from_list(Name, SecretsList),
    Permissions = get_value_from_list(Name, PermissionsList),
    MFARequired = get_value_from_list(Name, MFAList),
    case MFARequired of
        true ->
            NewListItem = {Name, IP, Secret, HandlerModule, lists:flatten([yubi_enabled, Permissions])};
        _ ->
            NewListItem = {Name, IP, Secret, HandlerModule, Permissions}
        end,

    compile_naslist(HandlerModule, T, SecretsList, PermissionsList, MFAList, [NewListItem | CompiledList]);

compile_naslist(_, [], _, _, _, CompiledList) ->
    CompiledList.


get_value_from_list(NasDeviceName, [{NasDeviceName, Value}|T]) ->
    Value;
get_value_from_list(NasDeviceName, [_H|T]) ->
    get_value_from_list(NasDeviceName, T);
get_value_from_list(_, []) ->
    not_found.


create_address_list([{["NAS",NasDeviceName,"address"],IPAddress}|T], Collector) -> 
    {ok, IP} = inet_parse:address(IPAddress),
    create_address_list(T, lists:append([{NasDeviceName, IP}], Collector));
create_address_list([_H|T], Collector) ->
    create_address_list(T, Collector);
create_address_list([], Collector) -> 
    Collector.

create_secret_list([{["NAS",NasDeviceName,"secret"],Secret}|T], Collector) -> 
    create_secret_list(T, lists:append([{NasDeviceName, list_to_binary(Secret)}], Collector));
create_secret_list([_H|T], Collector) ->
    create_secret_list(T, Collector);
create_secret_list([], Collector) -> 
    Collector.

create_permissions_list([{["NAS",NasDeviceName,"permissions"],PermList}|T], Collector) -> 
    TokenizedPermissions = [list_to_binary(X) || X <- string:tokens(PermList, ".")],
    create_permissions_list(T, lists:append([{NasDeviceName, TokenizedPermissions}], Collector));
create_permissions_list([_H|T], Collector) ->
    create_permissions_list(T, Collector);
create_permissions_list([], Collector) -> 
    Collector.


create_mfa_list([{["NAS",NasDeviceName,"requireMFA"],MFARequirement}|T], Collector) -> 
     create_mfa_list(T, lists:append([{NasDeviceName, MFARequirement}], Collector));
create_mfa_list([_H|T], Collector) ->
    create_mfa_list(T, Collector);
create_mfa_list([], Collector) -> 
    Collector.



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
