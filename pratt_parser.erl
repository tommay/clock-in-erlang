-module(pratt_parser).
-export([eval/1]).

-record(pratt_parser, {tokens}).
-define(is_pratt_parser(Term), is_record(Term, pratt_parser)).

-define(class(Rec), (begin {class, Module} = element(2, Rec), Module end)).

%% A Pratt parser.  Similar to a recursive decent parser but instead of
%% coding a function for each production, the syntax is coded in a set
%% of token objects that are yielded by the lexer.  New operators and
%% statements can be slipped in to the language with the proper
%% precedence by adding new token objects to the lexer without altering
%% the code for existing tokens.  Pretty cool.
%%
%% lexer is an enumerator with an each method that returns token objects
%% with three methods:
%% lbp: return the operator precedence.  Higher numbers bind more tightly.
%% nud(parser): called when the token is the first token in an expression,
%%   including a recursive call to expresssion (i.e., subexpression).  For
%%   Example, this would be called for a unary operator, a literal, or for
%%   the "if" in the construct "if <cond> then <expr>".
%%   It is the token's responsibility to call parser.expression, parser.expect,
%%   and/or parser.if? to handle the remainder of the expression, if any.
%% led(parser, left): called when the token is preceeded by a subexpression,
%%   left.  The token may be postfix or infix.
%%   It is the token's responsibility to call parser.expression, parser.expect,
%%   and/or parser.if? to handle the remainder of the expression, if any,
%%   and combine it with left.
%% Only lbp is mandatory.  nud and led will be called only when necessary, if
%% ever.
%% nud and lcd can call parser.expression(rbp) to recursively parse the
%% right expression.  rbp should be the token's lbp for left-associativity,
%% lbp-1 for right.
%%
%% PrattParser.new(lexer).eval will return the result of the parse.
%%
%% Syntax errors aren't handled at the moment and will cause ridiculous
%% exceptions to be raised such as NoMethodError.

%% http://javascript.crockford.com/tdop/tdop.html
%% http://effbot.org/zone/simple-top-down-parsing.htm
%% http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

eval(Tokens) when is_list(Tokens) ->
    PrattParser = new(Tokens),
    {_New, Value} = expression(PrattParser, 0),
    Value.

new(Tokens) when is_list(Tokens) ->
    #pratt_parser{tokens = Tokens}.

lookahead_token(_This = #pratt_parser{tokens = [Token | _Rest]}) ->
    Token;
lookahead_token(_This = #pratt_parser{tokens = []}) ->
    end_token:new().

token(This = #pratt_parser{tokens = [Token | Rest]}) ->
    {This#pratt_parser{tokens = Rest}, Token}.

%% Returns {NewThis, Value}.
expression(This, Rbp) ->
    {This2, Token} = token(This),
    ThisAndLeft = ?class(Token):nud(Token, This2),
    more_expression(ThisAndLeft, Rbp).

%% Returns {NewThis, Value}.
more_expression(ThisAndLeft = {This, Left}, Rbp) ->
    LookaheadToken = lookahead_token(This),
    case Rbp < ?class(LookaheadToken):lbp(LookaheadToken) of
	true ->
	    {This2, Token} = token(This),
	    This3AndLeft2 = ?class(Token):lcd(Token, This2, Left),
	    more_expression(This3AndLeft2, Rbp);
	false ->
	    ThisAndLeft
    end.
