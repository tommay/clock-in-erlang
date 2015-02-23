% Evaluate expressions using a PrattParser.

-module(pratt_evaluator).
-export([eval/1]).

-include("token.hrl").

eval(String) ->
    Tokens = tokenize(String),
    pratt_parser:eval(Tokens).

tokenize(String) ->
    lists:map(fun (Char) -> create_token(Char) end, String).

%% The simple clock expressions don't use parentheses.  These are just
%% here to see how easy it is to slip new rules into a grammar.  It is
%% indeed easy.  No need to modify existing rules or recompile a BNF
%% grammar.

create_token($() ->
    left_paren(0);
create_token($)) ->
    #token{type = right_paren, lbp = 0};

create_token($=) ->
    infix(10, '=', fun (Left, Right) -> Left == Right end);
create_token($+) ->
    infix(20, '+',  fun (Left, Right) -> Left + Right end);
create_token($-) ->
    infix(20, '-', fun (Left, Right) -> Left - Right end);
create_token($*) ->
    infix(30, '*', fun (Left, Right) -> Left * Right end);
create_token($/) ->
    infix(30, '/', fun (Left, Right) -> Left / Right end);
create_token($^) ->
    infix(40, '^', right, fun (Left, Right) -> math:pow(Left, Right) end);

left_paren(Lbp) ->
    #token{
       type = left_paren,
       lbp = Lbp,
       nud = fun (Parser) ->
		     {Parser2, Value} = pratt_parser:expression(Parser, Lbp),
		     Parser3 = pratt_parser:expect(NewParser, right_paren),
		     {Parser3, Value}.
	     end
      }.

infix(Lbp, Type, Func) ->
    infix(Lbp, Lbp, Type, Func).

infix(Lbp, Type, right, Func) ->
    infix(Lbp, Rbp - 1, Type, Func);
infix(Lbp, Rbp, Type, Func) ->
    #token{
       type = Type,
       lbp = Lbp,
       lcd = fun (This, Parser, Left) ->
		     {NewParser, Right} = pratt_parser:expression(Parser, Rbp),
		     {NewParser, Func(Left, Right)}
	     end
      }.

digit(Lbp, Digit) ->
    Value = list_to_integer(Digit),
    #token{
       type = digit,
       lbp = Lbp,
       nud = fun (This, Parser) -> {Parser, Value} end,
       lcd = fun (This, Parser, Left) -> {Parser, Left*10 + Value} end
      }.
