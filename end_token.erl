-module(end_token).
-export([new/0, lbp/1]).

-record(end_token, {class = {class, ?MODULE}}).
-define(is_end_token(Term), is_record(Term, end_token)).

new() ->
    #end_token{}.

lbp(This) when ?is_end_token(This) ->
    0.
