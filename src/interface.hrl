-record(get, {fromAccountId :: number() | all}).
-record(accountCreated,
{
  firstname :: binary(),
  surname :: binary(),
  account_number :: integer(),
  amount :: number()}).