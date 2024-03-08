---
title: Process links
assignment: mandatory
weight: 40
draft: false
---

<h2 class="subtitle">Mandatory assignment</h2>

In Erlang, processes can be [linked][lyse:process-links] to each other. If two
processes are linked to each other and one of them terminates abnormally, the
other one will automatically terminate.  

[lyse:process-links]: http://learnyousomeerlang.com/errors-and-processes#links

## File to use

In the file
`introduction-to-erlang/mandatory/src/link.erl` you find the following Erlang program.


``` Erlang
-module(link).
-export([start/0]).

start() ->
    spawn(fun() -> worker() end),
    timer:sleep(5000).

worker() ->
    timer:sleep(3000),
    exit(some_error).
```

The `start/0` function spawns a new process running the `worker/0` function and
then sleeps for 5 seconds. The `worker/0` function first sleeps for 3 seconds
and then terminates the calling process with the atom `some_error` as the exit
reason. 

{{% notice style="tip" title="Exit reason" %}}
In the above example, the atom `some_error` is used as the exit reason but you can
change this to any [valid Erlang term](http://erlang.org/doc/reference_manual/data_types.html).  
{{% /notice %}}


## Start an Erlang shell

In the terminal, navigate to the directory `introduction-to-erlang/mandatory/src`.

``` text
$ cd introduction-to-erlang/mandatory/src
```

Start an Erlang shell. 

``` text
$ erl
```

You should now see the Erlang shell starting. 

``` bash session
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:10] [kernel-poll:false]

Eshell V9.2  (abort with ^G)
erlang>

```

## Compile 

Compile the `link.erl` module. Don't forget end the expression with a trailing
`.` (dot).

``` bash session
erlang> c(link).
```

On success you now should see the following in the Erlang shell. 

``` erlang
{ok, link}
erlang>
```

## Run 

Run the program from the Erlang shell. 

``` erlang
erlang> link:start().
```

After 5 seconds both process has terminated and you should see the following in
the Erlang shell. 

``` erlang
ok
erlang>
```

Here `ok` means that the program terminated without errors. Although the worker
process terminated with reason `some_error` the parent process terminated
without errors. 

## Spawn and link

The built in function `spawn_link/1` can be used to spawn a new process and set
up a link between the calling process and the new process. Change `link.erl` to
use `spawn_link`. 

``` Erlang
-module(link).
-export([start/0]).

start() ->
    spawn_link(fun() -> worker() end),
    timer:sleep(5000).

worker() ->
    timer:sleep(3000),
    exit(some_error).
```

Compile and run. 

``` erlang
3> c(link).
{ok, link}
erlang> link:start().
```

After 3 seconds you should now see the following in the Erlang shell. 

``` erlang
** exception exit: some_error
erlang>
```

When a link is setup between two processes and one of the two processes
terminates, the other process automatically terminates with the same reason. 
Behind the scenes, the Erlang runtime sends an exit signal to terminate the
linked process.  

## Trap exit

A process can be set to trap the exit signal by setting the `trap_exit` process
flag to `true`. 

``` Erlang
process_flag(trap_exit, true)
```

A process that traps the exit signal will receive a message when a linked
process terminates. The message has the following format: 

``` Erlang
{'EXIT', PID, Reason}
```

, i.e., a 3-tuple where the first element is the atom 'EXIT', the second element
is the process id of the terminated process and the third element is the exit
reason. 

Make the parent process trap the exit signal. Also, make the parent process wait
for the exit message. 


``` Erlang
-module(link).
-export([start/0]).

start() ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> worker() end),

    receive
        {'EXIT', PID, Reason} ->
            io:format("Worker ~p terminated with reason ~w!~n", [PID, Reason])
    end.

worker() ->
    timer:sleep(3000),
    exit(some_error).
```

Compile and run. 

``` erlang
erlang> c(link).
{ok, link}
erlang> link:start().
```

After 3 seconds you should now see something similar to the following in the Erlang shell. 

``` erlang
Worker <0.79.0> terminated with reason some_error!
ok
erlang>
```

Now, when the worker process terminates, the parent process is notified by an
exit message. After receiving the exit message, the parent prints a short
message and then terminates normally. 





