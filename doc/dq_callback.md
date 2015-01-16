

# Module dq_callback #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`locks_leader`](/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md).

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###



<pre><code>
state() = #state{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-4">code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#elected-3">elected/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_leader-3">from_leader/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_DOWN-3">handle_DOWN/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-4">handle_call/4</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-3">handle_cast/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-3">handle_info/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_leader_call-4">handle_leader_call/4</a></td><td></td></tr><tr><td valign="top"><a href="#handle_leader_cast-3">handle_leader_cast/3</a></td><td>Called by leader in response to a <a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#leader_cast-2">leader_cast()</a>.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#merge_cand_states-2">merge_cand_states/2</a></td><td></td></tr><tr><td valign="top"><a href="#surrendered-3">surrendered/3</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-4"></a>

### code_change/4 ###


<pre><code>
code_change(FromVsn::string(), State::term(), Info::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>, Extra::term()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="elected-3"></a>

### elected/3 ###


<pre><code>
elected(State::<a href="#type-state">state()</a>, I::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>, Pid::pid() | undefined) -&gt; {ok, term(), <a href="#type-state">state()</a>} | {reply, term(), <a href="#type-state">state()</a>} | {ok, _AmLeaderMsg, _FromLeaderMsg, <a href="#type-state">state()</a>} | {error, term()}
</code></pre>
<br />


<a name="from_leader-3"></a>

### from_leader/3 ###


<pre><code>
from_leader(Op::term(), State::<a href="#type-state">state()</a>, I::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="handle_DOWN-3"></a>

### handle_DOWN/3 ###

`handle_DOWN(Pid, S, I) -> any()`


<a name="handle_call-4"></a>

### handle_call/4 ###


<pre><code>
handle_call(X1::term(), From::pid(), State::<a href="#type-state">state()</a>, I::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>) -&gt; {reply, term(), <a href="#type-state">state()</a>} | {noreply, <a href="#type-state">state()</a>} | {stop, term(), term(), <a href="#type-state">state()</a>} | {reply, term(), term(), <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="handle_cast-3"></a>

### handle_cast/3 ###


<pre><code>
handle_cast(Msg::term(), S::<a href="#type-state">state()</a>, I::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>) -&gt; {noreply, <a href="#type-state">state()</a>} | {stop, term(), term(), <a href="#type-state">state()</a>} | {reply, term(), <a href="#type-state">state()</a>} | {reply, term(), term(), <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="handle_info-3"></a>

### handle_info/3 ###


<pre><code>
handle_info(Msg::term(), State::<a href="#type-state">state()</a>, Info::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>) -&gt; {noreply, <a href="#type-state">state()</a>} | {stop, term(), term(), <a href="#type-state">state()</a>} | {reply, term(), <a href="#type-state">state()</a>} | {reply, term(), term(), <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="handle_leader_call-4"></a>

### handle_leader_call/4 ###


<pre><code>
handle_leader_call(Op::term(), From::pid(), State::<a href="#type-state">state()</a>, I::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>) -&gt; {reply, Reply, NState} | {reply, Reply, term(), NState} | {noreply, <a href="#type-state">state()</a>} | {stop, term(), Reply, NState}
</code></pre>
<br />


<a name="handle_leader_cast-3"></a>

### handle_leader_cast/3 ###


<pre><code>
handle_leader_cast(Msg::term(), S::term(), I::<a href="/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#type-leader_info">locks_leader:leader_info()</a>) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

Called by leader in response to a [leader_cast()](/Users/darach/Personal/GitHub/dq/deps/locks/doc/locks_leader.md#leader_cast-2).
<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Queue::queue()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="merge_cand_states-2"></a>

### merge_cand_states/2 ###

`merge_cand_states(Q, ListOfCandQs) -> any()`


<a name="surrendered-3"></a>

### surrendered/3 ###

`surrendered(State, X2, I) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###


<pre><code>
terminate(Reason::term(), State::<a href="#type-state">state()</a>) -&gt; ok
</code></pre>
<br />


