---
title: Stateful FIFO process
assignment: mandatory
weight: 30
draft: false
---

<h2 class="subtitle">Mandatory assignment</h2>

In the [fifo.erl](fifo) module you implemented a functional FIFO queue such
that, after each push or pop operation a new version of the queue is returned. An
alternative is to keep the immutable FIFO in a stateful Erlang process.

## State

A system is described as stateful if it is designed to remember preceding events
or user interactions; the remembered information is called the state of the
system. [^state]

[^state]: https://en.wikipedia.org/wiki/State_(computer_science)



## Stateful processes

A common pattern in Erlang is to provide stateful services as separate
processes. By spawning a process that executes a tail recursive function, the
arguments to the recursive function is used to hold the state of the stateful
process. In the recursive function, the process waits for messages to be
received. When a message is received, the process can change state by calling
the recursive function with an updated argument. 

### Self

The built in function `self()` returns the process id (PID) of the caller. 

### Example 

Study the following Erlang module and try to see if you can guess what
the functions does. 

``` erlang
-module(tick).

-export([start/0, tock/1, stop/1]).

start() ->
    spawn(fun() -> loop(0) end).

tock(Pid) ->
    Pid ! {tock, self()},
    receive
        T = {tick, _N} -> T
    end.

stop(Pid) ->
    Pid ! {stop, self()},
    receive
        {stop, ok} ->
            ok
    end.

loop(N) ->
    receive
        {tock, From}  ->
            From ! {tick, N},
            loop(N+1);
        {stop, From} ->
            From ! {stop, ok}
    end.
```

### Example explained 

A short explanation of the functions in the `tick` module. 

start()
: Spawns a process executing the recursive function `loop/1` with `0` as
  argument. The process id of the new process is returned. 

tock(Pid)
: Sends the message `{tock, self()}` to the process with process id `Pid` and
waits for a `{tick, _N} message in return.

stop(Pid)
: Sends the message `{stop, self()}` to the process with process id `Pid` and
  waits for the message `{stop, ok}` in return. 
  
loop(N)
: A recursive function used to wait for incoming messages. 

    - When the message `{tock , From}` is received, the message `{tick, N}` is
      sent in return to the process with process id `From` and then a recursive
      call `loop(N+1)` is made. The process executing the `loop/1` function has
      now changed state from `N` to `N+1`.

    - When the message `{stop , From}` is received, the message `{stop, ok}` is
      sent in return to the process with process id `From` and no recursive call
      is made. The process executing the `loop/1` function has now terminated.


### An example Erlang shell session. 

Try to understand the following Erlang shell session. 

``` erlang
1> T = tick:start().
<0.82.0>
2> tick:tock(T).
{tick,0}
3> tick:tock(T).
{tick,1}
4> tick:tock(T).
{tick,2}
5> tick:tock(T).
{tick,3}
6> tick:stop(T).
ok
7>
```

What conclusions can be made from the above Erlang shell session?

### Conclusions

The `tock/1` function now appears to have state. Calling `tock/1` with the same
argument `T` no longer returns the same value. The state is kept in a stateful
process. The `tock/1` and `stop/1` functions hides the message passing protocol
between the caller and the stateful process. 

## Statful FIFO process

If we keep the FIFO queue as a separate process, we can use the PID to the FIFO
process as a reference to the FIFO. When we update the state of the FIFO, the
PID remains the same. 


## File to use

Open the `introduction-to-erlang/mandatory/src/sfifo.erl` module. This module exports a number
of functions. The first of these exported functions is `sfifo:new/0` which
spawns a new process executing the `loop/1` function. The state of the `loop/1`
function is an immutable FIFO queue, i.e., an instance of the immutable FIFO
abstract data type you  implemented in the
`introduction-to-erlang/mandatory/src/fifo.erl` module.

## EDoc 

Look at the generated html documentation for the `sfifo.erl` module. 

## Messages and functional interface

To inspect and modify the state of the FIFO queue, messages can be sent to the
FIFO process and received in the `loop/1` function. 
It is good practice to hide the message passing protocol inside a functional
interface. The following functions are already implemented:

- `sfifo:new/0`
- `sfifo:size/1`
- `sfifo:empty/1`


{{% notice style="warning" title="Todo" %}}

Your task is to provide working implementations of the following interface
functions: 

- `sfifo:pop/1`
- `sfifo:push/2`

You will also need to make changes to the `loop/1` function. 

{{% /notice %}}

## How to handle popping an empty FIFO?

The immutable FIFO you implemented in `fifo.erl` throws an exception if you try
to pop an empty FIFO. 

The stateful FIFO you implement in `sfifo.erl` should not throw an exception if
you try to pop an empty FIFO, instead the tuple `{error, 'empty fifo'}` should
be returned. If you are not allowed to change `fifo.erl`, how can you accomplish
this?. 

{{% notice style="tip" title="Tip 1" %}}

In `sfifo:loop/1`, maybe you can use the `fifo:empty/1` function to test if the FIFO
is empty before using `fifo:pop/1`?

{{% /notice %}}

{{% notice style="tip" title="Tip 2" %}}

You cannot use `fifo:empty/1` together with the Erlang [if statement][if]. If
you do, you will get an error telling you that this will be an illegal guard.
The if statement only allows [guard sequences][guard-seq] and guards must be
free of side effects. Hence only a subset of valid Erlang expressions are
allowed in guards. Erlang cannot know if `fifo:empty/1` is free of any side
effects.

[if]: https://erlang.org/doc/reference_manual/expressions.html#if

[guard-seq]: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences

{{% /notice %}}

{{% notice style="tip" title="Tip 3" %}}

Use the Erlang [case expression][case] instead of the if expression. The case
expression allows for arbitrary **expressions** to be tested and matched similar
to how function clauses uses pattern matching and guards. 

[case]: https://erlang.org/doc/reference_manual/expressions.html#case
[guard-seq]: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences

{{% /notice %}}



## Test driven development

There are a number of EUnit test cases at the end of `sfifo.erl` file.
Repeat the following cycle.

<ol>
<li>
Compile
  <ol type="a">
    <li><code>$ make</code></li>
    <li> If error, make changes and goto 1.</li>
  </ol>
</li>  
<li>Runt the test:
   <ol type="a">
     <li><code>$ make test_sfifo</code></li>
     <li>If all tests passes, your are done :-)</li>
     <li>If one ore more tests fails, make a few changes and goto 1.</li>
   </ol>
</ol>

## First compile

To make sure you trigger a new compile, delete the following beam file. 

``` bash session
$ rm ebin/sfifo.beam
```

{{% notice title="Note" %}}

You don't have to delete the beam file before each compile in the future. The
make utility will automatically recompile if you made any changes to
`sfifo.erl`. 
{{% /notice %}}

Use make to compile the `src/sfifo.erl` module and create the resulting
`ebin/sfifo.beam` file. 


``` bash session
$ make
```

You will see a number of warnings:

``` bash session
src/sfifo.erl:14: Warning: opaque type sfifo() is not exported
src/sfifo.erl:65: Warning: variable 'Fifo' is unused
src/sfifo.erl:73: Warning: variable 'Fifo' is unused
src/sfifo.erl:73: Warning: variable 'Value' is unused
```

Warnings - but success! The warnings about unused variables will go away one
after another as you work your way through the assignment.


If you get the following warning, 

``` bash session
Warning: opaque type sfifo() is not exported
```

, export the opaque type `sfifo()` like this. 

``` Erlang
-opaque sfifo()::pid().
-export_type([sfifo/0]).
```


