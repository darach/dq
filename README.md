# **dq**  [![Build Status](https://travis-ci.org/darach/dq.png)](https://travis-ci.org/darach/dq)

> Distributed Fault Tolerant Queue

## Status

Experimental.

## Overview

**dq** is a distributed fault tolerant queue based on Ulf Wiger's and Thomas Arts deadlock-resolving [locks](http://github.com/uwiger/locks) library.

The queue exposes the core, extended and Okasaki queue APIs but the queue is redundant and uses distributed erlang to maintain consistent state on many nodes. The queue can detect new nodes and recover from lost nodes and netsplits. 

## Sample usage

A walkthrough of using the queue library.

**1. Start one or many distributed erlang nodes**

```
$ ERL_LIBS=deps erl -pa ebin -sname bill@darach -setcookie ted
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
(bill@darach)1>
```

**2. Ensure that the locks application is available and start it**

```
(bill@darach)1> application:ensure_all_started(locks).
{ok,[locks]}
(bill@darach)2>
```

**3. Create a distributed queue**

```
(bill@darach)2> {ok,Q} = dq:new(myq).
{ok,<0.46.0>}
(bill@darach)3>
```

**4. Start using the queue**

```
bill@darach)3> [ dq:in(X,Q) || X <- lists:seq(1,3) ].
[{[1],[]},{[2],[1]},{[3,2],[1]}]
(bill@darach)4>
```

**5. We can start other nodes at any time ...**

```
$ ERL_LIBS=deps erl -pa ebin -sname jill@darach -setcookie ted
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
(jill@darach)1> application:ensure_all_started(locks).
{ok,[locks]}
(jill@darach)2> {ok,Q} = dq:new(myq).
{ok,<0.46.0>}
(jill@darach)3> [ dq:in(X,Q) || X <- lists:seq(7,9) ].
[{[7],[]},{"\b",[7]},{"\t\b",[7]}]
(jill@darach)4>
```

Notice that the distributed queue on each node are distinct?
The nodes haven't been joined into a cluster and do not know about each other yet.

**6. Join nodes into a cluster**

```
(jill@darach)4> net_adm:ping(bill@darach).
pong
(jill@darach)5>
```
The distinct queues have now been merged into a single distributed queue.
This queue can be operated on from any of the nodes in the cluster.

```
(bill@darach)4> dq:in_r(777,Q).
{[9,8,7,3,2],[777,1]}
(bill@darach)5>
```

**7. There can be more than one distributed queue active in a cluster**

Create and populate a new queue on one node:

```
(bill@darach)5> {ok,P} = dq:new(otherq).
{ok,<0.54.0>}
(bill@darach)6> dq:in(1,P).
{[1],[]}
(bill@darach)7>
```

As the cluster is already active, we just need a reference to it on
the other cluster nodes to operate on it:

```
(jill@darach)5> {ok,P} = dq:new(otherq).
{ok,<0.56.0>}
(jill@darach)6> dq:in(2,P).
{[2],[1]}
(jill@darach)7>
```

## Enjoy!
