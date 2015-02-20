-module(dragon_snarl_handler).

-behaviour(eradius_server).
-export([radius_request/3]).

-include_lib("../include/eradius_lib.hrl").
-include_lib("../include/dictionary.hrl").
-include("../include/eradius_dict.hrl").


radius_request(R = #radius_request{cmd = request}, NasProp, [yubi_enabled|Opts]) ->
    radius_request(R, NasProp, [{yubi_enabled, true}|Opts]);

radius_request(R = #radius_request{cmd = request}, _NasProp, [{yubi_enabled, true}|RequestedPerms]) ->
    User = eradius_lib:get_attr(R, 1),
    UserPassAndOTP = string:strip(binary_to_list(eradius_lib:get_attr(R, 2)), both, $\0),
    UserPassOTPLength = string:len(UserPassAndOTP),
    if
        UserPassOTPLength > 44 ->
            UserPass = string:sub_string(UserPassAndOTP, 1, UserPassOTPLength - 44),
            OTP = string:sub_string(UserPassAndOTP, UserPassOTPLength - 43, UserPassOTPLength),
            check_authentication({User, RequestedPerms}, libsnarl:auth(User, list_to_binary(UserPass), list_to_binary(OTP)));
        true ->
            lager:warning("~s failed authentication. OTP required but not given.~n", [User]),
            {reply, #radius_request{cmd = reject, attrs = []}}
   end;              

radius_request(R = #radius_request{cmd = request}, _NasProp, [{yubi_enabled, false}|RequestedPerms]) ->
    User = eradius_lib:get_attr(R, 1),
    UserPass = list_to_binary(string:strip(binary_to_list(eradius_lib:get_attr(R, 2)), both, $\0)),
    check_authentication({User, RequestedPerms}, libsnarl:auth(User, UserPass));

radius_request(R = #radius_request{cmd = request}, NasProp, Opts) ->
    radius_request(R, NasProp, [{yubi_enabled, false}|Opts]);

radius_request(R = #radius_request{cmd = accreq}, _NasProp, _) ->
    lager:info("~nGOT ACCT REQUEST:~n~p~n", [R]);

radius_request(R, Nas, Opt) ->
    lager:info("~nUNHANDLED REQUEST:~n~p~n~p~n~p~n", [R, Nas, Opt]).



check_authentication({User, RequestedPerms}, {ok,{token,AuthToken}}) ->
   check_authorization({User, RequestedPerms}, libsnarl:allowed({token, AuthToken}, RequestedPerms));
  %  check_authorization({User, RequestedPerms}, libsnarl:allowed({token, AuthToken}, RequestedPerms));
check_authentication({User, _RequestedPerms}, {error,no_servers}) ->  
    lager:warning("~s failed authentication (No available Snarl server).~n", [User]),
    {reply, #radius_request{cmd = reject, attrs = []}};
check_authentication({User, _RequestedPerms}, _) ->
    lager:warning("~s failed authentication.~n", [User]),
    {reply, #radius_request{cmd = reject, attrs = []}}.


check_authorization({User, RequestedPerms}, true) ->
    lager:warning("~s granted ~p.~n", [User, RequestedPerms]),
    {reply, #radius_request{cmd = accept, attrs = []}};
check_authorization({User, RequestedPerms}, _) ->
    lager:warning("~s denied authorization to ~p.~n", [User, RequestedPerms]),
    {reply, #radius_request{cmd = reject, attrs = []}}.