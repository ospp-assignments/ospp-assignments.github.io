---
title: Random death
weight: 45
draft: false
---

Erlang can be used to construct very robust systems that can handle and recover
from unexpected errors. In order to test some of these features we need a way to
introduce random errors. 

## File to use

The module `introduction-to-erlang/mandatory/src/death.erl`  exports a single function `death:gamble(P)` that
will terminate the calling process with probability
0.0 &#x2264; P &#x2264; 1.0 using exit reason `random_death`.

## Start an Erlang shell

In the terminal, navigate to the directory `introduction-to-erlang/mandatory/src`.

``` bash session
$ cd introduction-to-erlang/mandatory/src
```

Start an Erlang shell. 

``` bash session
$ erl
```

You should now see the Erlang shell starting. 

``` erlang
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:10] [kernel-poll:false]

Eshell V9.2  (abort with ^G)
erlang>

```

## Compile 

Compile the `death.erl` module. Don't forget end the expression with a trailing
`.` (dot).

``` erlang
erlang> c(death).
```

On success you now should see the following in the Erlang shell. 

``` erlang
{ok, death}
erlang>
```

## Gamble with death

Call the `gamble/1` function with argument `1.0`. 

``` erlang
erlang> death:gamble(1.0).
```

The Erlang shell terminates with reason `random_death` and is automatically
restarted. 

``` erlang
** exception exit: random_death
     in function  death:gamble/1 (death.erl, line 34)
erlang>
```

Call the `gamble/1` function with argument `0.0`. 

``` erlang
erlang> death:gamble(0.0).
```

With death probability `0.0` the shell process will never be terminated, instead
the `gamble/1` functions returns the atom `ok`.

``` erlang
ok
erlang>
```

Experiment with other values for the death probability. For invalid
values for the death probability, for example 2.0 you will get the following
error message. 

``` erlang
** exception error: no function clause matching death:gamble(1.1) (death.erl, line 31)
```

