% Evaluate expressions using a PrattParser.

-module(pratt_evaluator).
-export([eval/1]).

-include("token.hrl").

eval(String) ->
    Tokens = tokenize(String),
    pratt_parser:eval(Tokens).

tokenize(String) ->
    Dict = create_tokens_dict(),
    lists:map(fun (Char) -> dict:fetch(Char, Dict) end, String).

create_tokens_dict() ->
    Tokens =
	[{$(, left_paren(0)},
	 {$), right_paren(0)},
	 {$=, infix(10, '=', fun (Left, Right) -> Left == Right end)},
	 {$+, infix(20, '+', fun (Left, Right) -> Left + Right end)},
	 {$-, infix(20, '-', fun (Left, Right) -> Left - Right end)},
	 {$*, infix(30, '*', fun (Left, Right) -> Left * Right end)},
	 {$/, infix(30, '/', fun (Left, Right) -> Left / Right end)},
	 {$^, infix(40, '^', right,
		    fun (Left, Right) -> math:pow(Left, Right) end)}],
    Dict = lists:foldl(
	     fun ({Char, Token}, Dict) ->
		     dict:store(Char, Token, Dict)
	     end,
	     dict:new(),
	     Tokens),
    lists:foldl(
      fun (Digit, AccumDict) ->
	      Token = digit(100, Digit),
	      dict:store($0 + Digit, Token, AccumDict)
      end,
      Dict,
      lists:seq(0, 9)).

%% The simple clock expressions don't use parentheses or
%% exponentiation.  These are just here to see how easy it is to slip
%% new rules into a grammar.  It is indeed easy.  No need to modify
%% existing rules or recompile a BNF grammar.

left_paren(Lbp) ->
    #token{
       type = left_paren,
       lbp = Lbp,
       nud = fun (Parser) ->
		     {Parser2, Value} =
			 pratt_parser:expression(Parser, Lbp),
		     Parser3 = pratt_parser:expect(Parser2, right_paren),
		     {Parser3, Value}
	     end
      }.

right_paren(Lbp) ->
    #token{type = right_paren, lbp = Lbp}.

infix(Lbp, Type, Func) ->
    infix(Lbp, Lbp, Type, Func).

infix(Lbp, Type, right, Func) ->
    infix(Lbp, Lbp - 1, Type, Func);
infix(Lbp, Rbp, Type, Func) ->
    #token{
       type = Type,
       lbp = Lbp,
       led = fun (Parser, Left) ->
		     {NewParser, Right} =
			 pratt_parser:expression(Parser, Rbp),
		     {NewParser, Func(Left, Right)}
	     end
      }.

digit(Lbp, Digit) ->
    #token{
       type = digit,
       lbp = Lbp,
       nud = fun (Parser) -> {Parser, Digit} end,
       led = fun (Parser, Left) -> {Parser, Left*10 + Digit} end
      }.
