%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([open_account/2, get_account/1, get_person/1 ]).


%% Opens an account, that is creates a new account containing a new person 
%% Writes them into database.

-spec open_account(binary(), binary()) -> #account{}.
open_account(Firstname, Surname) ->
    make_account(
      make_person(
        Firstname, Surname)
     ).

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNr) -> database:get_account(AccountNr).

-spec make_person(binary(), binary()) -> #person{}.
make_person(Firstname, Surname) ->
    PersId = database:unique_person_id(),
    Pers = #person{id = PersId,
                   firstname = Firstname,
                   surname = Surname},
    database:put_person(Pers),
    Pers.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> database:get_person(Id).

-spec make_account(#person{}) -> #account{}.
make_account(Person) ->
    AccNr = database:unique_account_number(),
    Acc = #account{account_number = AccNr,
                   person_id = Person#person.id,
                   amount = 1000},
    database:put_account(Acc),
    Acc.
