-module(pratt_parser).
-export([eval/1, expression/2, expect/2]).

-record(pratt_parser, {tokens}).
-define(is_pratt_parser(Term), is_record(Term, pratt_parser)).

-include("token.hrl").

%% A Pratt parser.  Similar to a recursive decent parser but instead
%% of coding a function for each production, the syntax is coded in a
%% set of token objects.  New operators and statements can be slipped
%% in to the language with the proper precedence by adding new token
%% objects to the lexer without altering the code for existing tokens.
%% Pretty cool.
%%
%% Tokens are records with three fields:
%% lbp: operator precedence.  Higher numbers bind more tightly.

%% nud: a fun, called as nud(Parser) when the token is the first
%%   token in an expression, including a recursive call to expresssion
%%   (i.e., subexpression).  For example, this would be called for a
%%   unary operator, a literal, or for the "if" in the construct "if
%%   <cond> then <expr>".  It is the token's responsibility to call
%%   pratt_parser:expression and/or pratt_parser:expect to handle the
%%   remainder of the expression, if any.
%%   Returns {NewParser, Value}.
%% led: fun, called as (Parser, Left) when the token is preceeded by a
%%   subexpression, Left.  The token may be postfix or infix.  It is the
%%   token's responsibility to call pratt_parser:expression and/or
%%   pratt_parser:expect to handle the remainder of the expression, if
%%   any, and combine it with Left.
%%   Returns {NewParser, Value}.
%%
%% Only lbp is mandatory.  nud and led will be called only when necessary,
%% if ever.
%% nud and led can call pratt_parser:expression(Parser, Rbp) to
%% recursively parse the right expression.  Rp should be the token's
%%  for left-associativity, bp-1 for right.
%%
%% pratt_parser:eval(Tokens) will return the result of the parse.
%%
%% Syntax errors aren't handled at the moment and will cause ridiculous
%% exceptions to be thrown.

%% http://javascript.crockford.com/tdop/tdop.html
%% http://effbot.org/zone/simple-top-down-parsing.htm
%% http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

%% Runs the Tokens through the parser and returns Value.
%%
eval(Tokens) when is_list(Tokens) ->
    PrattParser = new(Tokens),
    {_New, Value} = expression(PrattParser, 0),
    Value.

new(Tokens) when is_list(Tokens) ->
    #pratt_parser{tokens = Tokens}.

lookahead_token(_This = #pratt_parser{tokens = [Token | _Rest]}) ->
    Token;
lookahead_token(_This = #pratt_parser{tokens = []}) ->
    #token{type = end_token}.

token(This = #pratt_parser{tokens = [Token | Rest]}) ->
    {This#pratt_parser{tokens = Rest}, Token}.

%% Returns {NewThis, Value}.
%%
expression(This, Rbp) ->
    {This2, Token} = token(This),
    {This3, Left} = (Token#token.nud)(This2),
    more_expression(This3, Left, Rbp).

%% Returns {NewThis, Value}.
%%
more_expression(This, Left, Rbp) ->
    LookaheadToken = lookahead_token(This),
    case Rbp < LookaheadToken#token.lbp of
	true ->
	    {This2, Token} = token(This),
	    {This3, Left2} = (Token#token.led)(This2, Left),
	    more_expression(This3, Left2, Rbp);
	false ->
	    {This, Left}
    end.

%% If the next token is the ExpectedType the consume it otherwise
%% throw an exception.
%%
expect(This, ExpectedType) ->
    Type = (lookahead_token(This))#token.type,
    case Type == ExpectedType of
	true ->
	    {NewThis, _Token} = token(This),
	    NewThis;
	false ->
	    throw(spud:format("Expected ~s, got ~s", [ExpectedType, Type]))
    end.
