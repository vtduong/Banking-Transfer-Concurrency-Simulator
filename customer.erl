%%%-------------------------------------------------------------------
%%% @author vanduong
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2019 6:03 PM
%%%-------------------------------------------------------------------
-module(customer).
-author("vanduong").

%% API
-export([startCustomer/4]).

toString(Val)->
  lists:flatten(io_lib:format("~p", [Val])).

%% @doc starts a customer with given name, inital amount and bank list.
%% @param  ParentID : parent process id.
%% @param Name : customer name.
%% @param Amount : initial amount
%% @param BankList : list of banks
startCustomer(ParentID, Name, Amount, BankList)->
  timer:sleep(1000), % sleep 100 ms to make sure other processes are up
  borrow(ParentID, Name, Amount, BankList).

%% @doc randomly selects a bank from bank list and returns it.
%% @param BankList : list of banks
%% @returns a bank tuple or empty of list is empty
getRandomBank(BankList)->
  case BankList of
    [] -> {};
    _->
    lists:nth(rand:uniform(length(BankList)), BankList)
  end.

%% @doc randomly returns an amount between 1 and give amount.
%% @param Total : amount
%% @returns an amount
calculateLoan(Total)->
  if
    Total == 0->
      0;
    Total > 50->
      rand:uniform(50);
    true-> %match with anything
      rand:uniform(Total)
  end.

%% @doc checks if customer can/need to borrow more money, the process terminates if not.
%% @param ParentID : parent process id
%% @param Name : customer name
%% @param Total : current amount needs to loan
%% @param BankList : list of banks
checkValidity(ParentID, Name, Total, BankList)->
  if
    Total == 0-> %no need to borrow anymore
      ParentID ! {self(), {satisfied, Name, Total}},
      exit(normal);
    length(BankList) == 0-> %empty BankList, cannot borrow anymore
      ParentID ! {self(), {unsatisfied, Name, Total}},
      exit(normal);
    true->ok
  end.

%% @doc proceeds borrowing, updates total loan amount and bank list if needed.
%% @param ParentID : parent id process
%% @param Name : customer name
%% @param Total : current loan amount
%% @param BankLis : list of banks
borrow(ParentID, Name, Total, BankList) ->
  timer:sleep(rand:uniform(100) + 10), %sleep from 10-100 ms
  checkValidity(ParentID, Name, Total, BankList),
  {Bank, _} = getRandomBank(BankList),
  Loan = calculateLoan(Total),
  %send a message includes id, Name, amount to borrow, Bank
  ParentID ! {self(), {ask, Name, Loan, Bank}},
  %ParentID !  {toString(Name) ++ " started with " ++ toString(Amount) ++ " with bank list " ++ toString(BankList)}.
  {Banker, Amount} = get_feedback(),
  if
    Amount > 0->
      NewTotal = Total - Amount,
      borrow(ParentID, Name, NewTotal, BankList);
    true->
      %remove the bank from BankList
      borrow(ParentID, Name, Total, lists:keydelete(Banker,1, BankList))
  end.

%% @doc listens to message sent to this customer.
get_feedback() ->
  %if loan is granted, deduct loan amount from Amount, else remove bank from BankList
  receive
    {approved, {Banker, Loan}} ->
      {Banker, Loan};
    {rejected, {Banker, Loan}} ->
      {Banker, 0}

%%      get_feedback(ParentID, Name)
%%  after 2000 -> ParentID ! {"~w stops listening \n", [Name]}
  end.