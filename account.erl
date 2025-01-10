% modelam un cont bancar care are doua stari (active, closed)
% vrem sa implementam operatii: withdraw, deposit, check_balance, close_account

% in Erlang, gandim ca avem un automat de stari

% identificam care sunt procesele care participa (contul)
% identific care sunt actiunile si ***** schimba ele starile
% actiunile sunt mesaje! execut o actiune ***** ce primesc un mesaj ca trebuie sa execut actiunea respectiva


% atunci cand creez un cont nou
% starea acestui cont este active, si cu amount-ul initial 0
start_account() ->
  spawn(fun() -> account_loop(active, 0) end).

account_loop(State, Balance) ->
  receive
    {deposit, Amount, From} when State =:= active ->
      io:format("Se depun ~w~n", [Amount]),
      NewBalance = Balance + Amount,
      From ! {deposit_ok, NewBalance},
      account_loop(State, NewBalance);

    {withdraw, Amount, From} when State =:= active, Amount =< Balance ->
      io:format("Se retrag ~w~n", [Amount]),
      NewBalance = Balance - Amount,
      From ! {withdraw_ok, NewBalance},
      account_loop(State, NewBalance);

    {withdraw, Amount, From} when State =:= active ->
      io:format("Se incearca retragerea cu insucces garantat de ~w~n", [Amount]),
      From ! {error, insufficient_funds},
      account_loop(State, Balance);

    {check_balance, From} when State =:= active ->
      io:format("Check balance~n"),
      From ! {balance, Balance},
      account_loop(State, Balance);

    close_account when State =:= active ->
      io:format("Close account~n"),
      account_loop(closed, Balance);

    _ when State =:= closed ->
      io:format("This account is closed sir!~n"),
      account_loop(State, Balance);

    _ ->
      io:format("Invalid action!"),
      account_loop(State, Balance)

  end.

% am nodurile (active), respectiv (closed)
% ma gandesc la ***** arata o actiune
% deposit(Amount, From) -> { deposit, Amount, From }

main(_) ->
  AccountPid = start_account(),

  AccountPid ! {deposit, 100, self()},

  receive
    {deposit_ok, NewBalance} ->
      io:format("Depunere reusita, sold nou: ~p~n", [NewBalance])
  end,

  AccountPid ! {withdraw, 50, self()},

  receive
    {withdraw_ok, NB} ->
      io:format("Retragere reusita, sold nou: ~p~n", [NB]);
    {error, Reason} ->
      io:format("Eroare la retragere: ~p~n", [Reason])
  end,

  AccountPid ! {check_balance, self()},
  receive
    {balance, Balance} ->
      io:format("Soldul curent este: ~p~n", [Balance])
  end,

  AccountPid ! close_account,

  AccountPid ! {deposit, 50, self()},

  timer:sleep(500).
