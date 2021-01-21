-module(konto_test).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").

setup() ->
    database:init_database().

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun open_account/1]
     }}.

open_account(_) ->
    fun() ->
            Account = business_logic:open_account(<<"John">>, <<"Doe">>),
            {ok, Account_Check} = business_logic:get_account(Account#account.account_number),
            ?assertEqual(Account_Check#account.amount, 1000),
            {ok, Person_Check} = business_logic:get_person(Account_Check#account.person_id),
            ?assertEqual(Person_Check#person.surname, <<"Doe">>),
            ?assertEqual(Person_Check#person.firstname, <<"John">>)
    end.






