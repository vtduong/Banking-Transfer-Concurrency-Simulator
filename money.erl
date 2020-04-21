%%%-------------------------------------------------------------------
%%% @author vanduong
%%% @copyright (C) 2019, <COMPANY>
%%% @doc a program that simulates customer and banking loan system.
%%%
%%% @end
%%% Created : 11. Jun 2019 6:02 PM
%%%-------------------------------------------------------------------
-module(money).
-author("vanduong").

%% API
-export([start/0]).
%%% @doc starts the program.
start() ->
%%  CustomerList = element(2, file:consult("customers.txt")),
  CustomerList = element(2, file:consult("customers.txt")),
  io:fwrite("** Customers and loan objectives **~n"),
%%  printList(CustomerList),
  lists:map(fun({Key, Val})->io:fwrite("~p:~p~n", [Key, Val]) end, CustomerList),
%%  BankList = element(2, file:consult("banks.txt")),
  BankList = element(2, file:consult("banks.txt")),
  io:fwrite("** Banks and financial resources **~n"),
  lists:map(fun({Key, Val})->io:fwrite("~p:~p~n", [Key, Val]) end, BankList),
  %List of banks with id
  Banks = createBanks(BankList),
%%  io:fwrite(" Banks: ~p\n", [Banks]),
  %List of customers with id
  Customers = createCustomers(CustomerList, BankList),
%%  io:fwrite(" Customers: ~p\n", [Customers]),
%%  get_feedback(Banks, Customers),
  get_feedback(Banks, Customers, (length(Banks) + length(Customers))),
  ok.

%% @doc prints elements of any type in list.
%% @param List: the list to be printed.
printList(List)->
  case List of
    [] -> io:fwrite("\n");
    _->
      [Head | Tail] = List,
      io:fwrite("~p:~p\n", [element(1, Head), element(2, Head)]),
      printList(Tail)
  end.

%% @doc creates customer processes with a given customer list and bank list.
%% @param List : list of customers
%% @param BankList : list of banks
createCustomers(List, BankList)->
  case List == [] of
    true -> [];
    false ->
      [Head | Tail] = List,
      {Name, Amount} = Head,
      Id = spawn(customer, startCustomer, [self(), Name, Amount, BankList]),
%%    Temp = [Customers | {Name, Amount, Id}],
      Temp = [{Name, Amount, Id}],
      Temp ++ createCustomers(Tail, BankList)
  end.

%% @doc creates bank processes with a given list of banks.
%% @param List : list of banks
createBanks(List)->
  case List == [] of
    true -> [];
    false->
      [Head | Tail] = List,
      {Name, Amount} = Head,
      Id = spawn(bank, startBank, [self(),  element(1, Head), element(2, Head)]),
%%    Temp = [Customers | {Name, Amount, Id}],
      Temp = [{Name, Amount, Id}],
      Temp ++ createBanks(Tail)
  end.

get_feedback(Banks, Customers, NumAlive) ->
%%  is_process_alive(processid)
  %check if all processes have terminated
  if
    NumAlive =< 0->
      io:fwrite("DONE"),
      exit(normal);
    true ->
      receive
        {Sender, {ask, Cus, Loan, Bank}} ->
          io:fwrite(" ~p requests a loan of ~p dollars(s) from ~p\n ", [Cus, Loan, Bank]),
          %forward request to bank
          {_,_, BankID} = lists:keyfind(Bank, 1, Banks),
          BankID ! {bank, {Cus, Loan}},
          get_feedback(Banks, Customers, NumAlive);
        {Sender, {satisfied, Name, Total}}->
          {_, Amount, _} = lists:keyfind(Name, 1, Customers),
          Target = Amount - Total,
          io:fwrite(" ~p has reached the objective of ~p dollar(s). Yay!\n ", [Name, Target]),
          get_feedback(Banks, Customers, (NumAlive - 1));
        {Sender, {unsatisfied, Name, Total}} ->
          {_, Amount, _} = lists:keyfind(Name, 1, Customers),
          Remaining = Amount - Total,
          io:fwrite(" ~p was only able to borrow ~p dollar(s). Oh No!\n ", [Name, Remaining]),
          get_feedback(Banks, Customers, (NumAlive - 1));
        {Sender, {ok, Bank, Loaner, Result}}->
          %forward response to customer
          {_,_, Cid} = lists:keyfind(Loaner, 1, Customers),
          Cid ! {approved, {Bank, Result}},
          io:fwrite(" ~p approves a loan of ~p dollars from ~p \n",[Bank, Result, Loaner]),
          get_feedback(Banks, Customers, NumAlive);
        {Sender, {no, Bank, Loaner, Result}}->
          %forward response to customer
%%      io:fwrite("Bank, Loaner, Result: ~p ~p ~p\n",[Bank, Loaner, Result]),
%%      io:fwrite("what the heck: ~p\n",[lists:keyfind(Loaner, 1, Customers)]),
          {_,_, Cid} = lists:keyfind(Loaner, 1, Customers),
          Cid ! {rejected, {Bank, Result}},
          io:fwrite(" ~p denies a loan of ~p dollars from ~p \n",[Bank, Result, Loaner]),
          get_feedback(Banks, Customers, NumAlive);
        {Sender, {done, Bank, Total}} ->
          io:fwrite(" ~p has ~p dollar(s) remaining\n", [Bank, Total]),
          get_feedback(Banks, Customers, (NumAlive - 1))
%%  after 2000 -> true
      end
  end.


%% @doc listens to customer requests and bank response, co-ordinate between them.
%% @param Banks : list of banks
%% @param Customers : list of customers
%%get_feedback(Banks, Customers) ->
%%%%  is_process_alive(processid)
%%
%%  receive
%%    {Sender, {ask, Cus, Loan, Bank}} ->
%%      io:fwrite(" ~p requests a loan of ~p dollars(s) from ~p\n ", [Cus, Loan, Bank]),
%%      %forward request to bank
%%      {_,_, BankID} = lists:keyfind(Bank, 1, Banks),
%%      BankID ! {bank, {Cus, Loan}},
%%      get_feedback(Banks, Customers);
%%    {Sender, {satisfied, Name, Total}}->
%%      {_, Amount, _} = lists:keyfind(Name, 1, Customers),
%%      Target = Amount - Total,
%%      io:fwrite(" ~p has reached the objective of ~p dollar(s). Yay!\n ", [Name, Target]),
%%      get_feedback(Banks, Customers);
%%    {Sender, {unsatisfied, Name, Total}} ->
%%      {_, Amount, _} = lists:keyfind(Name, 1, Customers),
%%      Remaining = Amount - Total,
%%      io:fwrite(" ~p was only able to borrow ~p dollar(s). Oh No!\n ", [Name, Remaining]),
%%      get_feedback(Banks, Customers);
%%    {Sender, {ok, Bank, Loaner, Result}}->
%%      %forward response to customer
%%      {_,_, Cid} = lists:keyfind(Loaner, 1, Customers),
%%      Cid ! {approved, {Bank, Result}},
%%      io:fwrite(" ~p approves a loan of ~p dollars from ~p \n",[Bank, Result, Loaner]),
%%      get_feedback(Banks, Customers);
%%    {Sender, {no, Bank, Loaner, Result}}->
%%      %forward response to customer
%%%%      io:fwrite("Bank, Loaner, Result: ~p ~p ~p\n",[Bank, Loaner, Result]),
%%%%      io:fwrite("what the heck: ~p\n",[lists:keyfind(Loaner, 1, Customers)]),
%%      {_,_, Cid} = lists:keyfind(Loaner, 1, Customers),
%%      Cid ! {rejected, {Bank, Result}},
%%      io:fwrite(" ~p denies a loan of ~p dollars from ~p \n",[Bank, Result, Loaner]),
%%      get_feedback(Banks, Customers);
%%    {Sender, {done, Bank, Total}} ->
%%      io:fwrite(" ~p has ~p dollar(s) remaining\n", [Bank, Total]),
%%      get_feedback(Banks, Customers)
%%%%  after 2000 -> true
%%  end.