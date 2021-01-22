-module(account_service_test).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").
-include("interface.hrl").

setup() ->
    database:init_database(),
    {ok, Pid} = account_service:start(),
    business_logic:open_account(<<"person1 vn">>, <<"person1 nn">>),
    business_logic:open_account(<<"person2 vn">>, <<"person2 nn">>),
    business_logic:open_account(<<"person3 vn">>, <<"person3 nn">>),
    business_logic:open_account(<<"person4 vn">>, <<"person4 nn">>),
    Pid.

cleanup(Pid) -> gen_server:stop(Pid).

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun get_all_accounts/1, fun get_some_accounts/1, fun get_all_accounts_with_atom/1]
     }}.

get_all_accounts(_) ->
  fun() ->
    AccountCreatedList = gen_server:call(global:whereis_name(account_service), #get{fromAccountId = 0}),
    Count = length(AccountCreatedList),
    ?assertEqual(Count, 4)
  end.
get_all_accounts_with_atom(_) ->
  fun() ->
    AccountCreatedList = gen_server:call(global:whereis_name(account_service), #get{fromAccountId = all}),
    Count = length(AccountCreatedList),
    ?assertEqual(Count, 4)
  end.

get_some_accounts(_) ->
    fun() ->
            AccountCreatedList = gen_server:call(global:whereis_name(account_service), #get{fromAccountId = 3}),
            Count = length(AccountCreatedList),
            ?assertEqual(Count, 2)
    end.




