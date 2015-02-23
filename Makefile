# Dirt simple Makefile for a dirt simple Erlang prohect.

ERLS = $(wildcard *.erl)
BEAMS = $(ERLS:.erl=.beam)

%.beam: %.erl
	erlc $<

all: $(BEAMS)

pratt_parser.beam pratt_evaluator.beam: token.hrl

clean:
	rm *.beam