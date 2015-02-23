% Evaluate expressions using a PrattParser.

-module(pratt_evaluator).
-export([eval/1]).

-record(token, {type, lbp = 0, nud, lcd}).

eval(String) ->
    Tokens = tokenize(String),
    pratt_parser:eval(Tokens).

tokenize(String) ->
    lists:map(fun (Char) -> create_token(Char) end, String).

create_token($=) ->
    infix(10, fun (Left, Right) -> Left == Right end);
create_token($+) ->
    infix(20, fun (Left, Right) -> Left + Right end);
create_token($-) ->
    infix(20, fun (Left, Right) -> Left - Right end);
create_token($*) ->
    infix(30, fun (Left, Right) -> Left * Right end);
create_token($/) ->
    infix(30, fun (Left, Right) -> Left / Right end);
create_token($^) ->
    infix(40, right, fun (Left, Right) -> math:pow(Left, Right) end);
create_token($() ->
    left_paren(0);
create_token($)) ->
    #token{type = right_paren}.


left_paren(Lbp) ->
    #token{
       lbp = Lbp,
       nud = fun (Parser) ->
		     Result = pratt_parser:expression(Parser, Lbp),
		     pratt_parser:expect(right_paren),
		     Result
	     end
      }.


infix(Lbp, Func) ->
    infix(Lbp, Lbp, Func).

infix(Lbp, right, Func) ->
    infix(Lbp, Rbp - 1, Func);
infix(Lbp, Rbp, Func) ->
    #token{
       lbp = Lbp,
       lcd = fun (This, Parser, Left) ->
		     {NewParser, Right} = pratt_parser:expression(Parser, Rbp),
		     {NewParser, Func(Left, Right)}
	     end
      }.

digit(Lbp, Digit) ->
    Value = list_to_integer(Digit),
    #token{
       lbp = Lbp,
       nud = fun (This, Parser) -> {Parser, Value} end,
       lcd = fun (This, Parser, Left) -> {Parser, Left*10 + Value} end
      }.


    class InfixToken < Token
      def initialize(lbp, associates = :left, &block)
        super(lbp)
        @block = block
        @rbp = (associates == :left ? lbp : lbp - 1)
      end

      def led(parser, left)
        @block.call(left, parser.expression(@rbp))
      end
    end

    class DigitToken < Token
      def initialize(lbp, value)
        super(lbp)
        @value = value
      end
      
      def nud(parser)
        @value
      end
      
      def led(parser, left)
        left*10 + @value
      end
    end
    
    # The simple clock expressions don't use parentheses.  These are
    # just here to see how easy it is to slip new rules into a
    # grammar.  It is indeed easy.  No need to modify existing rules
    # or recompile a BNF grammar.
    
    class LeftParenToken < Token
      def nud(parser)
        parser.expression(lbp).tap do
          parser.expect(RightParenToken)
        end
      end
    end
    
    class RightParenToken < Token
    end
    
    @@tokens = {}
    
    def self.token(char, t)
      @@tokens[char] = t
    end

    def self.infix(char, lbp, associates = :left, &block)
      token(char, InfixToken.new(lbp, associates, &block))
    end

    token("(", LeftParenToken.new(0))
    token(")", RightParenToken.new(0))

    infix("=", 10, &:==)
    infix("+", 20, &:+)
    infix("-", 20, &:-)
    infix("*", 30, &:*)
    infix("/", 30, &:/)
    infix("^", 40, :right, &:**)

    (0..9).each do |d|
      token(d.to_s, DigitToken.new(100, d.to_f))
    end
  end
end
