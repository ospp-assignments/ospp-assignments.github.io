---
title: Process supervision
assignment: mandatory
weight: 50
draft: false
---

<h2 class="subtitle">Mandatory assignment</h2>

A parent process that traps the exit signal and spawns and links to a worker
process will be sent an exit message when the worker process terminates,  even if
the worker terminates due to an unexpected error. The parent process can now
restart the worker process if the worker terminates. The parent process is said
to be supervising the worker process, the parent is a process supervisor.

## System description

To study process supervision you will construct an Erlang system with one
supervisor process and one worker process called bang.

### Bang

Once every second, the bang process
decrements a counter and sends the counter value in a message to the supervisor.
When the counter reaches zero the bang process terminates with reason `bang`. To
simulate that the bang process may fail unexpectedly, once every second, the bang process will
gamble with death and with probability 30 % terminate with exit reason `random_death`.

### Supervisor

The supervisor process must trap the exit signal. To start and link to the bang process the
supervisor use `spawn_link/1`. If the bang process terminates, the supervisor
will receive an exit message including the PID and exit reason of the terminated process.
When receiving an exit message with reason
`random_death` the supervisor
should restart the bang process with the last received counter value. When receiving an exit message with reason
`bang` the supervisor
should print `>>BANG<<` and terminate normally. 

### Examples

In the below example, the supervisor with PID `<0.58.0>` starts a bang process
with PID `<0.98.0>` counting down from 5 to 0. After sending counter value 4
to the supervisor, the bang process randomly dies and is restarted by the
supervisor. When the bang process reaches counter value zero it terminates with
reason `bang` without sending the counter value to the supervisor. When the bang
process terminates with reason `bang` the supervisor prints `>>BANG<<` and
terminates normally. 

``` erlang
erlang> bang:start().

Supervisor with PID <0.58.0> started
bang(5) with PID <0.98.0> started
5 tick
4 tock
bang(3) with PID <0.98.0> died
bang(3) with PID <0.99.0> started
3 tick
2 tock
1 tick
0 tock
>>BANG<<
ok
erlang>
```

Another example where the bang process dies and is restarted by the supervisor
multiple times.

``` erlang
erlang> bang:start().

Supervisor with PID <0.58.0> started
bang(5) with PID <0.106.0> started
bang(5) with PID <0.106.0> died
bang(5) with PID <0.107.0> started
5 tick
4 tock
3 tick
bang(2) with PID <0.107.0> died
bang(2) with PID <0.108.0> started
bang(2) with PID <0.108.0> died
bang(2) with PID <0.109.0> started
2 tock
bang(1) with PID <0.109.0> died
bang(1) with PID <0.110.0> started
1 tick
0 tock
>>BANG<<
ok
erlang>
```

## File to use

In the `introduction-to-erlang/mandatory/src/bang.erl` module you find a skeleton of the system. 

## Compile 

Compile the `bang.erl` module. Don't forget to end the expression with a trailing
`.` (dot).

``` erlang
erlang> c(bang).
```

On success you now should see the following in the Erlang shell. 

``` erlang
{ok, bang}
erlang>
```

## Start the system 

Start the system from the Erlang shell. 

``` erlang
erlang> bang:start().
```

The system now starts and generated the following output. 

``` erlang
Supervisor with PID <0.133.0> started
bang(5) with PID <0.144.0> started
5 tick
4 tock
3 tick
2 tock
1 tick
0 tock
** exception exit: bang
```

## Trap exit

In `start/0`, make the supervisor trap the exit signal. 

## Random death

In `bang/2`, uncomment the call to `death:gamble/1`. 

## Handle exit messages

In `supervisor_loop/1`, make the supervisor handle the exit messages correctly.  
