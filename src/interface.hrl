-record(get, {fromAccountId :: number()}).
-record(accountCreated,
{
  firstname :: binary(),
  surname :: binary(),
  account_number :: integer(),
  amount :: number()}).