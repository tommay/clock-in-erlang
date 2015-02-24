clock_equations
===============

Figure out the simple equations that can be made from the time on a digital clock

My 9-year old daughter Chloe and I like to look at the digital clock
sometimes and make equations out of the numbers.  For example,
12:02 could be 1*2+0 = 2, and 12:03 could be 1+2 = 0+3.

I thought it would be fun to code up a program to find all the "clock
equations".  So I did.  First in Ruby, now in Erlang.

There are two parts: generating the equations, and evaluating their
truth value.

Generating the equations is done in clock.erl.

The equations' truth values are determined using an expression
evaluator (pratt_evaluator.erl) built on a generic Pratt parser
(pratt_parser.erl).  That was the fun part!
