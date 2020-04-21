%%%-------------------------------------------------------------------
%%% @author vanduong
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2019 6:03 PM
%%%-------------------------------------------------------------------
-module(bank).
-author("vanduong").

%% API
-export([startBank/3]).

toString(Val)->
  lists:flatten(io_lib:format("~p", [Val])).

startBank(ParentID, Name, Total)->
  timer:sleep(1000), %sleep 100 ms to make sure other processes are up
  lend(ParentID, Name, Total).

getLoan(Loan, Total)->
  %compare loan & amount
  %returns 0 if loan is greater than amount
  if
    (Loan > Total)->0;
    true ->Loan
  end.

lend(ParentID, Name, Total)->
  {Loaner, Loan} = get_request(ParentID, Name, Total),
  %check if requested loan can be granted
%%  Result = getLoan(Loan, Total),
  if
    (Loan =< Total)->
      %send a message includes bank id, bank name, customerID, customer name, result
      ParentID ! {self(), {ok, Name, Loaner, Loan}},
      lend(ParentID, Name, Total - Loan);
    true ->
%%      io:fwrite("Name, Loaner, Loan: ~p ~p ~p\n",[Name, Loaner,Loan]),
      %send a message includes bank id, bank name, customerID, customer name, result
      ParentID ! {self(), {no, Name, Loaner, Loan}},
      lend(ParentID, Name, Total)
  end.


get_request(ParentID, Name, Total) ->
  receive
    {bank, {Cus, Loan}} ->
%%      io:fwrite("Bank ~p received request of ~p from ~p\n",[Name, Loan, Cus]),
      {Cus, Loan}
%%      get_request(ParentID, Name, Total)
  after 3000 ->
    ParentID ! {self(), {done, Name, Total}},
    exit(normal)
  end.