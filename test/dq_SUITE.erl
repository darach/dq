%% -------------------------------------------------------------------
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
%% -------------------------------------------------------------------
%%  @author Darach Ennis <darach.ennis@gmail.com>
%%  @copyright (C) 2014, Darach Ennis
%%
%%  @doc
%% 
%%  @end
%% -------------------------------------------------------------------
-module(dq_SUITE).

-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([groups/0]).

-export([t_new_anon/1]).
-export([t_new_named/1]).
-export([t_usage/1]).
-export([t_netsplitheal/1]).

-include_lib("common_test/include/ct.hrl").

-define(proptest(TC), proper:quickcheck(TC,[{numtests,10000}])
                        orelse ct:fail({counterexample, proper:counterexample(TC)})).

all() ->
    [
        {group, dq}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        {dq, [], [
            t_new_anon,
            t_new_named,
            t_usage,
            t_netsplitheal
            ]}
    ].

init_per_suite(Config) ->
    {ok,Apps} = application:ensure_all_started(locks),
    [ {apps,Apps} | Config ].

end_per_suite(Config) ->
    {apps,Apps} = lists:keyfind(apps,1,Config),
    [ application:stop(App) || App <- Apps ],
    Config.

t_new_anon(_Config) ->
    {ok,A} = dq:new(),
    {ok,B} = dq:new(),
    true = A =/= B,
    0 = dq:len(A),
    0 = dq:len(B),
    true = dq:is_queue(A),
    true  = dq:is_queue(B),
    dq:in(1,A),
    {[1],[]} = dq:snapshot(B),
    dq:in(2,B),
    {[2],[1]} = dq:snapshot(B),
    {[2],[1]} = dq:snapshot(A),
    ok.

t_new_named(_Config) ->
    {ok,A} = dq:new(),
    true = dq:is_leader(A),
    {ok,B} = dq:new(),
    false = dq:is_leader(B),
    {ok,C} = dq:new(jed,[]),
    {ok,_} = dq:new(ned,[{debug,true}]),
    {ok,_} = dq:new(bed,[{debug,false}]),
    {ok,_} = dq:new(sed,[]),
    true = dq:is_leader(C),
    true = A =/= B,
    true = A =/= C,
    0 = dq:len(A),
    0 = dq:len(B),
    0 = dq:len(C),
    true = dq:is_empty(A),
    true = dq:is_empty(B),
    true = dq:is_empty(C),
    true = dq:is_queue(A),
    true = dq:is_queue(B),
    true = dq:is_queue(C),
    dq:in(1,A),
    {[1],[]} = dq:snapshot(A),
    {[1],[]} = dq:snapshot(B),
    {[],[]} = dq:snapshot(C),
    false = dq:is_empty(A),
    false = dq:is_empty(B),
    true = dq:is_empty(C),
    dq:in(2,B),
    {[2],[1]} = dq:snapshot(A),
    {[2],[1]} = dq:snapshot(B),
    {[],[]} = dq:snapshot(C),
    dq:in(3,C),
    {[2],[1]} = dq:snapshot(A),
    {[2],[1]} = dq:snapshot(B),
    {[3],[]} = dq:snapshot(C),
    false = dq:is_empty(C),
    ok.

t_usage(_Config) ->
    {ok,A} = dq:new(),
    {ok,B} = dq:new(),
    {ok,C} = dq:new(other),
    dq:in(2,A),
    dq:in_r(1,A),
    {{value,1},{[],[2]}} = dq:out(A),
    {{value,2},{[],[]}} = dq:out_r(A),
    {empty,{[],[]}} = dq:out(A),
    L = lists:seq(1,9),
    [ dq:in(X,A) || X <- L ],
    L = queue:to_list(dq:snapshot(B)),
    R = lists:reverse(L),
    dq:reverse(A),
    R = queue:to_list(dq:snapshot(B)),
    {{[5,6,7,8],[9]},{[1],[4,3,2]}} = dq:split(5,A),
    R = queue:to_list(dq:snapshot(B)),
    _X = dq:join(A,queue:in(10,queue:new())),
    [9,8,7,6,5,4,3,2,1,10] = queue:to_list(dq:snapshot(B)),
    {[10],[8,6,4,2]} = dq:filter(fun(I) -> I rem 2 == 0 end, A),
    true = dq:member(10,A),
    true = dq:member(2,A),
    false = dq:member(11,A),
    9 = dq:get(B),
    10 = dq:get_r(B),
    empty = dq:get_r(C),
    empty = dq:get(C),
    {[10],[8,7,6,5,4,3,2,1]} = dq:drop(A),
    {[1,2,3],[8,7,6,5,4]} = dq:drop_r(A),
    empty = dq:drop(C),
    empty = dq:drop_r(C),
    {value,8} = dq:peek(B),
    {value,1} = dq:peek_r(B),
    empty = dq:peek(C),
    empty = dq:peek_r(C),
    {[1,2,3],[11,8,7,6,5,4]} = dq:cons(11,B),
    11 = dq:head(A),
    empty = dq:head(C),
    {[1,2,3],[8,7,6,5,4]} = dq:tail(A),
    empty = dq:tail(C),
    {[12,1,2,3],[11,8,7,6,5,4]} = dq:snoc(B,12),
    12 = dq:daeh(A),
    empty = dq:daeh(C),
    12 = dq:last(B),
    empty = dq:last(C),
    {[1,2,3],[11,8,7,6,5,4]} = dq:liat(A),
    empty = dq:liat(C),
    {[2,3],[11,8,7,6,5,4]} = dq:init(B),
    empty = dq:init(C),

    % last, but not least, make sure both instances are equivalent
    {[2,3],[11,8,7,6,5,4]} = dq:snapshot(A),
    {[2,3],[11,8,7,6,5,4]} = dq:snapshot(B),

    ok.

t_netsplitheal(_Config) ->
    %% We let lock_leaders hang wet and assume it just works,
    %% But, we need to test the candidate state merge of queues
    %% which is done here...
    %% @TODO Find evidence of easy to orchestrate multi-node CT tests for inspiration ...

    CandidateStates = [{c, {true, {[1,2],[3]}}}, {c, false}, {c, {true, {[3,4],[5]}}}],

    % 3 is in common and is considered duplicate, and so removed, ...
    {[4,5,1,2,3,666],[777]} = dq_callback:merge_cand_states({[666],[777]},CandidateStates),

    ok.
