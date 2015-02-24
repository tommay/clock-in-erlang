-module(clock).
-export([main/0]).
-compile(export_all).

%% My 9-year old daughter Chloe and I like to look at the digital clock
%% sometimes and make equations out of the numbers.  For example,
%% 12:02 could be 1*2+0 = 2, and 12:03 could be 1+2 = 0+3.
%%
%% I thought it would be fun to code up a program to find all the
%% "clock eauations".  There are two parts: generating the equations,
%% and evaluating their truth value.
%%
%% I originally did it in Ruby then thought it would be fun to do in
%% Erlang.  I was right.

main() ->
    Times = times(),
    lists:foreach(
      fun (Time) ->
	      Equations = equations_for_time(Time),
	      lists:foreach(
		fun (Equation) ->
			case pratt_evaluator:eval(Equation) of
			    true ->
				io:format("~s => ~s~n", [Time, Equation]);
			    false ->
				void
			end
		end,
		Equations)
      end,
      Times).

%% Returns a list of times, ["1:00", "1:01", ..., "12:59"].
%%
times() ->
    lists:flatmap(
      fun (Hour) ->
	      lists:map(
		fun (Minute) ->
			lists:flatten(io_lib:format("~w:~.2.0w", [Hour, Minute]))
		end,
		lists:seq(0, 59))
      end,
      lists:seq(1, 12)).

%% Takes a time "1:01" and returns a list of all the well-formed
%% equations that can be made from the time.
%%
equations_for_time(Time) ->
    %% Get rid of the colon.

    TimeDigits = [D || D <- Time, D /= $:],
  
    %% We're going to take the time and intersperse the digits with
    %% all combinations of operators.  Ops is the array of all
    %% possible operator combinations of the correct length to
    %% intersperse with the time.

    OpCombinations = combinations(
		       length(TimeDigits) - 1,
		       ["=", "+", "-", "*", ""]),

    %% There must be exactly one equals sign.

    ValidOpCombinations =
	lists:filter(
	  fun (OpCombination) ->
		  length([E || E <- OpCombination, E == "="]) == 1
	  end,
	  OpCombinations),
  
    %% Zip each operator set into the digits and flatten the result to
    %% make a "string'.

    Lumpy = lists:map(
      fun (Ops) ->
	      lists:zipwith(
		fun (Op, Digit) ->
			[Op, Digit]
		end,
		["" | Ops],   % No operator before the initial digit.
		TimeDigits)
      end,
      ValidOpCombinations),

    [lists:flatten(E) || E <- Lumpy].

%% Returns all combinations of length N of the elements in List.  Each
%% combination will use each element zero to N times.
%%
combinations(N, List) when is_integer(N), is_list(List) ->
    [H | T] = lists:duplicate(N, List),
    C = lists:foldl(
	  fun (E, Accum) ->
		  cons_product(Accum, E)
	  end,
	  %% Listify the elements for the initial accumulator so they
	  %% can be consed onto.
	  [[E] || E <- H],
	  T),
    %% Reverse the lists because things look better that way :-).
    [lists:reverse(E) || E <- C].

%% A is a list of lists.  Returns a new list of lists created
%% by consing each element of B onto each element of A.
%%
cons_product(A, B) ->
    lists:flatmap(
      fun (AE) ->
	      lists:map(fun (BE) -> [BE | AE] end, B)
      end,
      A).
