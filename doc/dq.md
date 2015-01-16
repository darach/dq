

# Module dq #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-dq">dq()</a> ###


__abstract datatype__: `dq()`




### <a name="type-item">item()</a> ###



<pre><code>
item() = any()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cons-2">cons/2</a></td><td>Equivalent to queue:cons/2.</td></tr><tr><td valign="top"><a href="#daeh-1">daeh/1</a></td><td>Equivalent to queue:daeh/1.</td></tr><tr><td valign="top"><a href="#drop-1">drop/1</a></td><td>Equivalent to queue:drop/1.</td></tr><tr><td valign="top"><a href="#drop_r-1">drop_r/1</a></td><td>Equivalent to queue:drop_r/1.</td></tr><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td>Equivalent to queue:filter/2.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Equivalent to queue:get/1.</td></tr><tr><td valign="top"><a href="#get_r-1">get_r/1</a></td><td>Equivalent to queue:get_r/1.</td></tr><tr><td valign="top"><a href="#head-1">head/1</a></td><td>Equivalent to queue:head/1.</td></tr><tr><td valign="top"><a href="#in-2">in/2</a></td><td>Equivalent to queue:in/2.</td></tr><tr><td valign="top"><a href="#in_r-2">in_r/2</a></td><td>Equivalent to queue:in_r/2.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>Equivalent to queue:init/1.</td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td>Equivalent to queue:is_empty/1.</td></tr><tr><td valign="top"><a href="#is_leader-1">is_leader/1</a></td><td>Is this queue instance the global distributed queue leader?.</td></tr><tr><td valign="top"><a href="#is_queue-1">is_queue/1</a></td><td>Equivalent to queue:is_queue/1.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>Equivalent to queue:join/2.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td>Equivalent to queue:last/1.</td></tr><tr><td valign="top"><a href="#len-1">len/1</a></td><td>Equivalent to queue:len/1.</td></tr><tr><td valign="top"><a href="#liat-1">liat/1</a></td><td>Equivalent to queue:liat/1.</td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td>Equivalent to queue:member/2.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create a new global distributed anonymous queue.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create a global distributed queue with name.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Create a global distributed queue with name and options.</td></tr><tr><td valign="top"><a href="#out-1">out/1</a></td><td>Equivalent to queue:out/1.</td></tr><tr><td valign="top"><a href="#out_r-1">out_r/1</a></td><td>Equivalent to queue:out_r/1.</td></tr><tr><td valign="top"><a href="#peek-1">peek/1</a></td><td>Equivalent to queue:peek/1.</td></tr><tr><td valign="top"><a href="#peek_r-1">peek_r/1</a></td><td>Equivalent to queue:peek_r/1.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>Equivalent to queue:reverse/1.</td></tr><tr><td valign="top"><a href="#snapshot-1">snapshot/1</a></td><td>Get a snapshot of the distributed queue's current state.</td></tr><tr><td valign="top"><a href="#snoc-2">snoc/2</a></td><td>Equivalent to queue:snoc/2.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td>Equivalent to queue:split/2.</td></tr><tr><td valign="top"><a href="#tail-1">tail/1</a></td><td>Equivalent to queue:tail/1.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cons-2"></a>

### cons/2 ###


<pre><code>
cons(Item::<a href="#type-item">item()</a>, Queue::<a href="#type-dq">dq()</a>) -&gt; queue()
</code></pre>
<br />

Equivalent to queue:cons/2.
<a name="daeh-1"></a>

### daeh/1 ###


<pre><code>
daeh(Queue::<a href="#type-dq">dq()</a>) -&gt; queue() | empty
</code></pre>
<br />

Equivalent to queue:daeh/1.
<a name="drop-1"></a>

### drop/1 ###


<pre><code>
drop(Queue::<a href="#type-dq">dq()</a>) -&gt; queue() | empty
</code></pre>
<br />

Equivalent to queue:drop/1.
<a name="drop_r-1"></a>

### drop_r/1 ###


<pre><code>
drop_r(Queue::<a href="#type-dq">dq()</a>) -&gt; queue() | empty
</code></pre>
<br />

Equivalent to queue:drop_r/1.
<a name="filter-2"></a>

### filter/2 ###


<pre><code>
filter(Fun::fun((<a href="#type-item">item()</a>) -&gt; boolean()), Queue::<a href="#type-dq">dq()</a>) -&gt; queue()
</code></pre>
<br />

Equivalent to queue:filter/2.
<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a> | empty
</code></pre>
<br />

Equivalent to queue:get/1.
<a name="get_r-1"></a>

### get_r/1 ###


<pre><code>
get_r(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a> | empty
</code></pre>
<br />

Equivalent to queue:get_r/1.
<a name="head-1"></a>

### head/1 ###


<pre><code>
head(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a> | empty
</code></pre>
<br />

Equivalent to queue:head/1.
<a name="in-2"></a>

### in/2 ###


<pre><code>
in(Item::<a href="#type-item">item()</a>, Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a>
</code></pre>
<br />

Equivalent to queue:in/2
<a name="in_r-2"></a>

### in_r/2 ###


<pre><code>
in_r(Item::<a href="#type-item">item()</a>, Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a>
</code></pre>
<br />

Equivalent to queue:in_r/2
<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a> | empty
</code></pre>
<br />

Equivalent to queue:init/1.
<a name="is_empty-1"></a>

### is_empty/1 ###


<pre><code>
is_empty(Queue::<a href="#type-dq">dq()</a>) -&gt; boolean()
</code></pre>
<br />

Equivalent to queue:is_empty/1
<a name="is_leader-1"></a>

### is_leader/1 ###


<pre><code>
is_leader(Queue::<a href="#type-dq">dq()</a>) -&gt; boolean()
</code></pre>
<br />

Is this queue instance the global distributed queue leader?
<a name="is_queue-1"></a>

### is_queue/1 ###


<pre><code>
is_queue(Queue::pid()) -&gt; boolean()
</code></pre>
<br />

Equivalent to queue:is_queue/1
<a name="join-2"></a>

### join/2 ###


<pre><code>
join(Queue::<a href="#type-dq">dq()</a>, Q2::queue()) -&gt; queue()
</code></pre>
<br />

Equivalent to queue:join/2.
<a name="last-1"></a>

### last/1 ###


<pre><code>
last(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a> | empty
</code></pre>
<br />

Equivalent to queue:last/1.
<a name="len-1"></a>

### len/1 ###


<pre><code>
len(Queue::<a href="#type-dq">dq()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Equivalent to queue:len/1
<a name="liat-1"></a>

### liat/1 ###


<pre><code>
liat(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a> | empty
</code></pre>
<br />

Equivalent to queue:liat/1.
<a name="member-2"></a>

### member/2 ###


<pre><code>
member(Item::<a href="#type-item">item()</a>, Queue::<a href="#type-dq">dq()</a>) -&gt; boolean()
</code></pre>
<br />

Equivalent to queue:member/2.
<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; <a href="#type-dq">dq()</a>
</code></pre>
<br />

Create a new global distributed anonymous queue
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Name::term()) -&gt; <a href="#type-dq">dq()</a>
</code></pre>
<br />

Create a global distributed queue with name
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Name::term(), Opts::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="#type-dq">dq()</a>
</code></pre>
<br />

Create a global distributed queue with name and options
<a name="out-1"></a>

### out/1 ###


<pre><code>
out(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a>
</code></pre>
<br />

Equivalent to queue:out/1
<a name="out_r-1"></a>

### out_r/1 ###


<pre><code>
out_r(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a>
</code></pre>
<br />

Equivalent to queue:out_r/1
<a name="peek-1"></a>

### peek/1 ###


<pre><code>
peek(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a>
</code></pre>
<br />

Equivalent to queue:peek/1.
<a name="peek_r-1"></a>

### peek_r/1 ###


<pre><code>
peek_r(Queue::<a href="#type-dq">dq()</a>) -&gt; <a href="#type-item">item()</a>
</code></pre>
<br />

Equivalent to queue:peek_r/1.
<a name="reverse-1"></a>

### reverse/1 ###


<pre><code>
reverse(Queue::<a href="#type-dq">dq()</a>) -&gt; queue()
</code></pre>
<br />

Equivalent to queue:reverse/1.
<a name="snapshot-1"></a>

### snapshot/1 ###


<pre><code>
snapshot(Queue::<a href="#type-dq">dq()</a>) -&gt; queue()
</code></pre>
<br />

Get a snapshot of the distributed queue's current state
<a name="snoc-2"></a>

### snoc/2 ###


<pre><code>
snoc(Queue::<a href="#type-dq">dq()</a>, Item::<a href="#type-item">item()</a>) -&gt; queue()
</code></pre>
<br />

Equivalent to queue:snoc/2.
<a name="split-2"></a>

### split/2 ###


<pre><code>
split(N::pos_integer(), Queue::<a href="#type-dq">dq()</a>) -&gt; {queue(), queue()}
</code></pre>
<br />

Equivalent to queue:split/2.
<a name="tail-1"></a>

### tail/1 ###


<pre><code>
tail(Queue::<a href="#type-dq">dq()</a>) -&gt; queue() | empty
</code></pre>
<br />

Equivalent to queue:tail/1.
