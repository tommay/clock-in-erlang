-module(clock).
-export([main/0]).

%% My 9-year old daughter Chloe and I like to look at the digital clock
%% sometimes and make equations out of the numbers.  For example,
%% 12:02 could be 1*2+0 = 2, and 12:03 could be 1+2 = 0+3.
%%
%% I thought it would be fun to code up a program to find all the
%% "clock eauations".  There are two parts: generating the equations,
%% and evaluating their truth value.

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

%% Returns the product of two lists, something any self-respecting
%% functional language should already have.
%%
product(A, B) ->
    lists:flatmap(
      fun (AE) ->
	      lists:map(fun (BE) -> [AE, BE] end, B)
      end,
      A).

%% Returns all combinations of length N of the elements in List.  Each
%% combination will use each element zero to N times.
%%
combinations(N, List) when is_integer(N), is_list(List) ->
    [H | T] = lists:duplicate(List),
    A = lists:fold(
	  fun (E, Accum) ->
		  product(Accum, E)
	  end,
	  H, T),
    [lists:flatten(E) || E <- A].

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

    OpCombinations = combinations(length(time_digits) - 1, ["=", "+", "-", "*", "/", ""]),
  
    %% Zip each operator set into the digits and join the result to
    %% make a string.

    Lumpy = lists:map(
      fun (Ops) ->
	      lists:zipwith(
		fun (Op, Digit) ->
			[Op, Digit]
		end,
		["" | Ops],
		TimeDigits)
      end,
      OpCombinations),

    [lists:flatten(E) || E <- Lumpy].

def time_and_equations
  # Generate each time.
  # Both the ranges need to use .lazy to get full laziness.

  times = (1..12).lazy.flat_map do |hour|
    (0..59).lazy.map do |minute|
      "%d:%02d" % [hour, minute]
    end
  end

  # Create all equations for each time:
  # [[time0, equation0], [time0, equation1], ..., [timeN, equation0], ...]

  time_and_equations = times.flat_map do |time|
    [time].product(equations_for_time(time))
  end

  # Select the valid time/equations.

  valid_time_and_equations = time_and_equations.select do |time, equation|
    PrattEvaluator.eval(equation)
  end

  # Return them.

  valid_time_and_equations
end

def equations_for_time(time)
  time_digits = time.sub(/:/, "").chars
  
  # We're going to take the time and intersperse the digits with
  # all combinations of operators.  Except we only allow one "=".
  # ops is the array of all possible operator combinations of the
  # correct length to intersperse with the time.
  
  ops = ([["=", "+", "-", "*", "/", ""]] * (time_digits.size - 1))
    .reduce(&:product)
    .map(&:flatten)
    .select{|x| x.count("=") == 1}
  
  # Zip each operator set into the digits and join the result to
  # make a string.  If the string evaluates as true, print it out.
  
  ops.map do |op_array|
    time_digits.zip(op_array).flatten.join
  end
end

if __FILE__ == $0
  main
end
