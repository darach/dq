%% -------------------------------------------------------------------
%% Copyright (c) 2015 Darach Ennis < darach at gmail dot com > 
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% File: dq.erl. Distributed Queue API. Follows the standard Erlang queue API.
%%
%% -------------------------------------------------------------------
-module(dq).

-export([new/0]).
-export([new/1]).
-export([new/2]).
-export([is_leader/1]).
-export([snapshot/1]).

-export([is_queue/1]).
-export([is_empty/1]).
-export([len/1]).

-export([in/2]).
-export([in_r/2]).

-export([out/1]).
-export([out_r/1]).

-export([reverse/1]).
-export([split/2]).
-export([join/2]).

-export([filter/2]).
-export([member/2]).

-export([get/1]).
-export([get_r/1]).
-export([drop/1]).
-export([drop_r/1]).
-export([peek/1]).
-export([peek_r/1]).

-export([cons/2]).
-export([head/1]).
-export([tail/1]).
-export([snoc/2]).
-export([daeh/1]).
-export([last/1]).
-export([liat/1]).
-export([init/1]).

-opaque dq() :: pid().
-type item() :: any().

-export_type([dq/0]).
-export_type([item/0]).

-spec new() -> dq().
%% @doc Create a new global distributed anonymous queue
new() ->
    locks_leader:start_link(dq_callback, queue:new()).

-spec new(term()) -> dq().
%% @doc Create a global distributed queue with name
new(Name) ->
    new(Name,[]).

-spec new(term(),proplists:proplist()) -> dq().
%% @doc Create a global distributed queue with name and options
new(Name,Opts) ->
    Debug = lists:keyfind(debug,1,Opts),
    case Debug of
        {debug,true} ->
            dbg:tracer(),
            dbg:tpl(locks_leader,x),
            dbg:tpl(locks_agent,x),
            dbg:tpl(dq_callback,x),
            dbg:tpl(dq,x),
            dbg:p(all,[c]);
        _Other ->
            no_debug
    end,
    locks_leader:start_link(Name, dq_callback, queue:new(), Opts).

-define(timeout, 2000).
-define(r(Queue,Expr,L), locks_leader:leader_call(Queue, {read, fun(Q) -> Expr end}, ?timeout)).
-define(p(Queue,Expr,L), locks_leader:leader_call(Queue, {pop, fun(Q) -> Expr end}, ?timeout)).
-define(w(Queue,Expr,L), locks_leader:leader_call(Queue, {write, fun(Q) -> Expr end}, ?timeout)).

-spec is_queue(pid()) -> boolean().
%% @doc Equivalent to queue:is_queue/1
is_queue(Queue) ->
    ?r(Queue, queue:is_queue(Q), is_queue).

-spec is_empty(dq()) -> boolean().
%% @doc Equivalent to queue:is_empty/1
is_empty(Queue) ->
    ?r(Queue, queue:is_empty(Q), is_empty).

-spec len(dq()) -> non_neg_integer().
%% @doc Equivalent to queue:len/1
len(Queue) ->
    ?r(Queue, queue:len(Q), len).

-spec in(item(), dq()) -> item().
%% @doc Equivalent to queue:in/2
in(Item, Queue) ->
    ?w(Queue, queue:in(Item,Q), in).

-spec in_r(item(), dq()) -> item().
%% @doc Equivalent to queue:in_r/2
in_r(Item, Queue) ->
    ?w(Queue, queue:in_r(Item,Q), in_r).

-spec out(dq()) -> item().
%% @doc Equivalent to queue:out/1
out(Queue) ->
    ?p(Queue, queue:out(Q), out).

-spec out_r(dq()) -> item().
%% @doc Equivalent to queue:out_r/1
out_r(Queue) ->
    ?p(Queue, queue:out_r(Q), out_r).

-spec reverse(dq()) -> queue().
%% @doc Equivalent to queue:reverse/1.
reverse(Queue) ->
    ?w(Queue, queue:reverse(Q), reverse).

-spec split(pos_integer(), dq()) -> {queue(),queue()}.
%% @doc Equivalent to queue:split/2.
split(N, Queue) ->
    ?r(Queue, queue:split(N, Q), split).

-spec join(dq(), queue()) -> queue().
%% @doc Equivalent to queue:join/2.
join(Queue,Q2) ->
    ?w(Queue, queue:join(Q, Q2), join).

-spec filter(fun((item()) -> boolean()), dq()) -> queue(). 
%% @doc Equivalent to queue:filter/2.
filter(Fun, Queue) ->
    ?r(Queue, queue:filter(Fun, Q), filter).

-spec member(item(), dq()) -> boolean().
%% @doc Equivalent to queue:member/2.
member(Item, Queue) ->
    ?r(Queue, queue:member(Item, Q), member).

-spec get(dq()) -> item() | empty.
%% @doc Equivalent to queue:get/1.
get(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?r(Queue, queue:get(Q), get)
    end.

-spec get_r(dq()) -> item() | empty.
%% @doc Equivalent to queue:get_r/1.
get_r(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?r(Queue, queue:get_r(Q), get_r)
    end.

-spec drop(dq()) -> queue() | empty.
%% @doc Equivalent to queue:drop/1.
drop(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?w(Queue, queue:drop(Q), drop)
    end.

-spec drop_r(dq()) -> queue() | empty.
%% @doc Equivalent to queue:drop_r/1.
drop_r(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?w(Queue, queue:drop_r(Q), drop_r)
    end.

-spec peek(dq()) -> item().
%% @doc Equivalent to queue:peek/1.
peek(Queue) ->
    ?r(Queue, queue:peek(Q), peek).

-spec peek_r(dq()) -> item().
%% @doc Equivalent to queue:peek_r/1.
peek_r(Queue) ->
    ?r(Queue, queue:peek_r(Q), peek_r).

-spec cons(item(),dq()) -> queue().
%% @doc Equivalent to queue:cons/2.
cons(Item,Queue) ->
    ?w(Queue, queue:cons(Item,Q), cons).

-spec head(dq()) -> item() | empty.
%% @doc Equivalent to queue:head/1.
head(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?r(Queue, queue:head(Q), head)
    end.

-spec tail(dq()) -> queue() | empty.
%% @doc Equivalent to queue:tail/1.
tail(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?r(Queue, queue:tail(Q), tail)
    end.

-spec snoc(dq(),item()) -> queue().
%% @doc Equivalent to queue:snoc/2.
snoc(Queue,Item) ->
    ?w(Queue, queue:snoc(Q,Item), snoc).

-spec daeh(dq()) -> queue() | empty.
%% @doc Equivalent to queue:daeh/1.
daeh(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?r(Queue, queue:daeh(Q), daeh)
    end.

-spec last(dq()) -> item() | empty.
%% @doc Equivalent to queue:last/1.
last(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?r(Queue, queue:last(Q), last)
    end.

-spec liat(dq()) -> item() | empty.
%% @doc Equivalent to queue:liat/1.
liat(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?w(Queue, queue:liat(Q), liat)
    end.

-spec init(dq()) -> item() | empty.
%% @doc Equivalent to queue:init/1.
init(Queue) ->
    case is_empty(Queue) of
        true -> empty;
        false -> ?w(Queue, queue:init(Q), init)
    end.

-spec is_leader(dq()) -> boolean().
%% @doc Is this queue instance the global distributed queue leader?
is_leader(Queue) ->
    InfoList = locks_leader:info(Queue),
    {leader,LeaderPid} = lists:keyfind(leader,1,InfoList),
    {leader_node,LeaderNode} = lists:keyfind(leader_node,1,InfoList),
    Queue =:= LeaderPid andalso LeaderNode =:= node().

-spec snapshot(dq()) -> queue().
%% @doc Get a snapshot of the distributed queue's current state
snapshot(Queue) ->
    InfoList = locks_leader:info(Queue),
    {mod_state,{state,_,QueueState}} = lists:keyfind(mod_state,1,InfoList),
    QueueState.
