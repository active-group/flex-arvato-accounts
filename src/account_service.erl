-module(account_service).

-behaviour(gen_server).
-include("data.hrl").
-include("interface.hrl").
-export([init/1, handle_cast/2, handle_call/3,
  start/0, read_all_accounts_and_filter/1, get/1]).


start() ->
  {ok, Pid} = gen_server:start(?MODULE, 0, []),
  global:register_name(account_service, Pid),
  {ok, Pid}.





get(FromAccountId) -> gen_server:call(global:whereis_name(account_service), #get{ fromAccountId = FromAccountId }).
%get(Pid) -> gen_server:call(Pid, #get{fromAccountId = 0}).

-spec read_all_accounts_and_filter(number() | all) -> list(#accountCreated{}).
read_all_accounts_and_filter(FromAccountId) ->
  if
    FromAccountId == all -> perform_read_all_accounts_and_filter(0);
    true -> perform_read_all_accounts_and_filter(FromAccountId)
  end.

perform_read_all_accounts_and_filter(FromAccountId) ->
  FilteredAccounts = business_logic:get_all_accounts_from(FromAccountId),
  Result = lists:map( fun(Account) ->
    {ok, ThePerson} = business_logic:get_person(Account#account.person_id),
    #accountCreated{
      account_number = Account#account.account_number,
      amount = Account#account.amount,
      firstname = ThePerson#person.firstname,
      surname = ThePerson#person.surname
    } end, FilteredAccounts),
  lager:info("Selected ~w AccountCreated Events with FromAccountId ~w~n", [length(Result) | [FromAccountId]]),
  Result
.


init(_A) -> {ok, _A}. % gibt initialen Zustand zurück

handle_cast(_A, _B) -> {noreply, _A}.

handle_call(#get{
  fromAccountId = FromAccountId
}, _From, _OLD) ->
  lager:info("Getting Request to deliver created accounts from id ~w~n", [FromAccountId]),
  Reply = read_all_accounts_and_filter(FromAccountId),
  {reply, Reply, _OLD}.

