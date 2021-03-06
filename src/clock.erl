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
			try pratt_evaluator:eval(Equation) of
			    true ->
				io:format("~s => ~s~n", [Time, Equation]);
			    false ->
				void
			catch
			    error: badarith ->
				void
			end
		end,
		Equations)
      end,
      Times).

%% Returns a list of times, ["1:00", "1:01", ..., "12:59"].
%%
times() ->
    [lists:flatten(io_lib:format("~w:~.2.0w", [Hour, Minute])) ||
	Hour <- lists:seq(1, 12),
	Minute <- lists:seq(0, 59)].

%% Takes a time "1:01" and returns a list of all the well-formed
%% equations that can be made from the time.
%%
equations_for_time(Time) ->
    %% Get rid of the colon.

    TimeDigits = [D || D <- Time, D /= $:],
  
    %% We're going to take the time and intersperse the digits with
    %% all combinations of operators.  OpCombinations is the array of
    %% all possible operator combinations of the correct length to
    %% intersperse with the time.

    OpCombinations = combinations(
		       length(TimeDigits) - 1,
		       ["=", "+", "-", "*", "/", "%", "^", ""]),

    %% There must be exactly one equals sign.

    ValidOpCombinations =
	lists:filter(
	  fun (OpCombination) ->
		  length([E || E <- OpCombination, E == "="]) == 1
	  end,
	  OpCombinations),
  
    %% Zip each operator set into the digits and flatten the result to
    %% make a "string'.

    Lumpy =
	[lists:zipwith(
	   fun (Op, Digit) ->
		   [Op, Digit]
	   end,
	   ["" | Ops],   % No operator before the initial digit.
	   TimeDigits)
	 || Ops <- ValidOpCombinations],

    [lists:flatten(E) || E <- Lumpy].

%% Returns all combinations of length N of the elements in List.  Each
%% combination will use each element zero to N times.  If N is zero
%% then the result is [[]] because there is one combination with zero
%% elements and it is empty.
%%
combinations(N, List) when is_integer(N), is_list(List) ->
    C = combinations(N, [[]], List),
    %% Reverse each combination because things look better that way.
    [lists:reverse(E) || E <- C].

combinations(0, Accum, _List) ->
    Accum;
combinations(N, Accum, List) ->
    NewAccum = lists:flatmap(
	    fun (C) ->
		    [[E | C] || E <- List]
	    end,
	    Accum),
    combinations(N - 1, NewAccum, List).
