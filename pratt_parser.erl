We can do this easily enough without using an actor for the lexer.
Just need to work out the token/next_token stuff.  A lexer is
initialized with a list of tokens.  It might as well just *be* a list
of tokens.



-module(pratt_parser).
-export([]).

-record(pratt_parser, {class = {class, ?MODULE}, lexer, token}).

-define(class(Rec), begin {class, Module} = element(2, Rec), Module end).

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

new(Lexer) ->
    #pratt_parser(lexer = Lexer, lookahead_token = undefined).


class PrattParser
  def initialize(lexer)
    @lexer = Enumerator.new do |y|
      lexer.each do |token|
        y << token
      end
      y << EndToken.new
    end

    @token = nil
  end


lookahead_token(This) ->
    This#pratt_parser.lookahead_token.

token(This) ->
    Token = lookahead_token(This),
    NextToken = lexer:next(This#pratt_parser.lexer),
    {This#pratt_parser{token = NextToken}, Token}.

eval(This) ->
    {This2, _Token} = token(This),
    expression(This2, 0).

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
	    ThisAndLeft2 = ?class(Token):lcd(Token, This2, Left),
	    more_expression(ThisAndLeft2, Rbp);
	false ->
	    ThisAndLeft
    end.

  class EndToken
    def lbp
      0
    end
  end
end
