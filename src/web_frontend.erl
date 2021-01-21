
-module(web_frontend).
-include("data.hrl").
-export([init/2]).



account_opened_success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
       <a href=\"/\"> Back </a>
    " >>.


account_open_form() ->
            << "
<h3> Open Account </h3>
               <form method=\"post\" action=\"/accounts/open\">
  <label for=\"accounts_firstname\"> Firstname </label>
  <input type=\"text\" id=\"accounts_firstname\" name=\"accounts_firstname\" />

  <label for=\"accounts_secondname\"> Secondname </label>
  <input type=\"text\" id=\"accounts_secondname\" name=\"accounts_secondname\" />

  <input type=\"submit\" value=\"Open account\" />
</form>" >>.


index() ->
    io_lib:format("~s",
                  [account_open_form()]).

%% /accounts/open
init(Req, open_account) ->

    lager:info("Creating new account"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    Firstname = maps:get(<<"accounts_firstname">>, KeyValues),
    Secondname = maps:get(<<"accounts_secondname">>, KeyValues),

    Account = business_logic:open_account(Firstname, Secondname),
    Body = io_lib:format(account_opened_success(), [Account#account.account_number]),

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),

    lager:info("Created account with account number ~p", [Account#account.account_number]),

    {ok, Req2, []};

%% /index
init(Req0, index) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Req0),
    {ok, Req, []}.
