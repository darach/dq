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
%% File: dq_callback.erl Distributed Queue API locks_leader behaviour callback
%%
%% -------------------------------------------------------------------
-module(dq_callback).
-behaviour(locks_leader).

-export([init/1]).
-export([elected/3]).
-export([surrendered/3]).
-export([handle_DOWN/3]).
-export([handle_leader_call/4]).
-export([handle_leader_cast/3]).
-export([from_leader/3]).
-export([handle_call/4]).
-export([handle_cast/3]).
-export([handle_info/3]).
-export([terminate/2]).
-export([code_change/4]).

%% for testing
-export([merge_cand_states/2]).

-type queue() :: any().

-record(state, {
  am_leader = false :: boolean(),
  queue = undefined :: undefined | queue()
}).

-type state() :: #state{}.

-define(event(E), event(?LINE, E)).

-spec init(queue()) -> {ok, state()}.
init(Queue) ->
    ?event({init, Queue}),
    {ok, #state{queue = Queue}}.

-spec elected(state(), locks_leader:leader_info(), pid() | undefined) ->
   {ok, term(), state()}
 | {reply, term(), state()}
 | {ok, _AmLeaderMsg, _FromLeaderMsg, state()}
 | {error, term()}.
elected(#state{queue = Queue} = S, I, undefined) ->
    ?event(elected_leader),
    case locks_leader:new_candidates(I) of
    [] ->
        ?event({elected, Queue}),
        {ok, {sync, Queue}, S#state{am_leader = true}};
    Cands ->
        ?event({new_candidates, Cands}),
        NewQueue = merge_candidates(Queue, I),
        {ok, {sync, NewQueue}, S#state{am_leader = true, queue = NewQueue}}
    end;
elected(#state{queue = Queue} = S, _E, Pid) when is_pid(Pid) ->
    {reply, {sync, Queue}, S#state{am_leader = true}}.

merge_candidates(Q, I) ->
    {Good, _Bad} = locks_leader:ask_candidates(merge, I),
    merge_cand_states(Q, Good).

merge_cand_states(Q, ListOfCandQs) ->
    lists:foldl(
        fun({C, {true, Q2}}, Acc) ->
            ?event({merge_got, C, Q2}),
            merge(Acc, Q2);
        ({C, false}, Acc) ->
            ?event({merge_got, C, false}),
            Acc
        end, Q, ListOfCandQs).

merge(Q1,Q2) ->
    case queue:out(Q2) of
        {empty, _Q} ->
            Q1;
        {{value,V}, Q3} -> 
            case queue:member(V,Q1) of
                true -> merge(Q1, Q3);
                false -> merge(queue:in(V,Q1), Q3)
            end
    end.

surrendered(#state{queue = _OurQueue} = S, {sync, LeaderQueue}, _I) ->
    ?event({surrendered, LeaderQueue}),
    {ok, S#state{queue = LeaderQueue, am_leader = false}}.

handle_DOWN(_Pid, S, _I) ->
    {ok, S}.

-spec handle_leader_call(term(), pid(), state(), locks_leader:leader_info()) -> 
    {reply, Reply, NState} |
    {reply, Reply, term(), NState} |
    {noreply, state()} |
    {stop, term(), Reply, NState}.

handle_leader_call({read,F} = Op, _From, #state{queue = Queue} = S, _I) ->
    ?event({handle_leader_call, Op}),
    Reply = F(Queue),
    {reply, Reply, {read, F}, S#state{queue = Queue}};
handle_leader_call({pop,F} = Op, _From, #state{queue = Queue} = S, _I) ->
    ?event({handle_leader_call, Op}),
    {_Value, NewQueue}=Ret = F(Queue),
    {reply, Ret, {pop, F}, S#state{queue=NewQueue}};
handle_leader_call({write,F} = Op, _From, #state{queue = Queue} = S, _I) ->
    ?event({handle_leader_call, Op}),
    NewQueue = F(Queue),
    {reply, NewQueue, {write, F}, S#state{queue = NewQueue}};
handle_leader_call({leader_lookup,F} = Op, _From, #state{queue = Queue} = S, _I) ->
    ?event({handle_leader_call, Op}),
    Reply = F(Queue),
    {reply, Reply, S#state{queue = Queue}}.

-spec handle_leader_cast(term(), term(), locks_leader:leader_info()) -> 
    {ok, state()}.
%% @doc Called by leader in response to a {@link locks_leader:leader_cast/2. leader_cast()}.
handle_leader_cast(_Msg, S, _I) ->
    ?event({handle_leader_cast, _Msg}),
    {ok, S}.

-spec from_leader(term(), state(), locks_leader:leader_info()) -> {ok, state()}.
from_leader({sync, D}, #state{} = S, _I) ->
    {ok, S#state{queue = D}};
from_leader({read,_F} = Op, #state{queue = Queue} = S, _I) ->
    ?event({from_leader, Op}),
    {ok, S#state{queue = Queue}};
from_leader({write,F} = Op, #state{queue = Queue} = S, _I) ->
    ?event({from_leader, Op}),
    NewQueue = F(Queue),
    {ok, S#state{queue = NewQueue}};
from_leader({pop,F} = Op, #state{queue = Queue} = S, _I) ->
    ?event({from_leader, Op}),
    {_Value, NewQueue} = F(Queue),
    {ok, S#state{queue = NewQueue}}.

-spec handle_call(term(), pid(), state(), locks_leader:leader_info()) ->
    {reply, term(), state()}
  | {noreply, state()}
  | {stop, term(), term(), state()}
  | {reply, term(), term(), state()}.
handle_call(merge, _From, #state{am_leader = AmLeader, queue = Queue} = S, _I) ->
    case AmLeader of
        true -> {reply, {true, Queue}, S};
        false -> {reply, false, S}
    end;
handle_call({read, F}, _From, #state{queue = Queue} = S, _I) ->
    Reply = F(Queue),
    {reply, Reply, S}.

-spec handle_cast(term(), state(), locks_leader:leader_info()) ->
      {noreply, state()}
    | {stop, term(), term(), state()}
    | {reply, term(), state()}
    | {reply, term(), term(), state()}.
handle_cast(_Msg, S, _I) ->
    {noreply, S}.

-spec handle_info(term(), state(), locks_leader:leader_info()) ->
      {noreply, state()}
    | {stop, term(), term(), state()}
    | {reply, term(), state()}
    | {reply, term(), term(), state()}.
handle_info(_Msg, State, _Info) ->
    {noreply, State}.

-spec code_change(string(), term(), locks_leader:leader_info(), term()) ->
    {ok, state()}.
code_change(_FromVsn, State, _Info, _Extra) -> {ok, State}.

-spec terminate(term(),state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec event(integer(), term()) -> ok.
event(_Line, _Event) -> ok.
