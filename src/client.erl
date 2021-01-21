%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([open_account/2]).

%% opens an acocunt with a given first and surname.
%% prints the result and the account number to stdout.
-spec open_account(binary(), binary()) -> ok.
open_account(Firstname, Surname) ->
    Account = business_logic:open_account(Firstname, Surname),
    io:format("Account was successfully opened. Account number: ~p ~n", [Account#account.account_number]).

