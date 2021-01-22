-module(account_service).

-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_cast/2, handle_call/3,
  start/0, read_all_accounts_and_filter/1, get/2, get/1]).


start() ->
  {ok, Pid} = gen_server:start(?MODULE, 0, []),
  global:register_name(account_service, Pid),
  {ok, Pid}.


-record(get, {fromAccountId :: number()}).
-record(accountCreated,
{
  firstname :: binary(),
  surname :: binary(),
  account_number :: integer(),
  amount :: number()}).


get(Pid, FromAccountId) -> gen_server:call(Pid, #get{ fromAccountId = FromAccountId }).
get(Pid) -> gen_server:call(Pid, #get{fromAccountId = 0}).

-spec read_all_accounts_and_filter(number()) -> list(#accountCreated{}).
read_all_accounts_and_filter(FromAccountId) ->
  FilteredAccounts = business_logic:get_all_accounts_from(FromAccountId),
  lists:map( fun(Account) ->
    {ok, ThePerson} = business_logic:get_person(Account#account.person_id),
    #accountCreated{
    account_number = Account#account.account_number,
    amount = Account#account.amount,
    firstname = ThePerson#person.firstname,
    surname = ThePerson#person.surname
  } end, FilteredAccounts)
  .


init(_A) -> {ok, _A}. % gibt initialen Zustand zurück

handle_cast(_A, _B) -> {noreply, _A}.

handle_call(#get{
  fromAccountId = FromAccountId
}, _From, _OLD) ->
  Reply = read_all_accounts_and_filter(FromAccountId),
  {reply, Reply, _OLD}.

