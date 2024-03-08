---
title: Statitistics (v2)
weight: 60
draft: true
---

The next step is to modify the game server in order to keep track of the number
of incorrect and correct guesses. Let's use a tagged tuple of the form `{stats,
NumRight, NumWrong}` to keep track of the number of correct and incorrect
guesses.

## game_loop_v2/2

In the file `game.erl`, take a look at the `game_loop_v2/2`  function. The second
argument to this function is a tuple `{stats, NumRight, NumWrong}`. To update the
stats tuple the function `update_stats_v2/2` is used. 

### Todo

You must make a small change to `update_stats_v2/2`. 

## test/1

In the file `game.erl`, take a look at the `test/1` function. This is a wrapper
function used to call `test/4` with default parameters. 

## Compile from the Erlang shell

Compile the updated module from the Erlang shell. 

{{< highlight erlang >}}
12> c(game).
{ok, game}
13>
{{< /highlight >}}

You will still get a few warnings. 

## Test from the Erlang shell

Let's try the `test/1` function. 

{{< highlight erlang >}}
13> S2 = game:test(v2).
<0.226.0>
P1 [1 , 10] X =/= 5 
P1 [1 , 10] X =/= 5 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 1 
P1 [1 , 10] X =/= 9 
P1 [1 , 10] X =/= 9 
P1 [1 , 10] X =/= 6 
P1 [1 , 10] X =/= 8 
P1 [1 , 10] X =/= 1 
P1 [1 , 10] X =/= 5 
P1 [1 , 10] X =/= 8 
P1 [1 , 10] X =/= 2 
P1 [1 , 10] X =/= 9 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 10 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 8 
P1 [1 , 10] X =/= 6 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 8 
P1 [1 , 10] X =:= 3 (22 guesses, PID = <0.227.0>)
14> 
{{< /highlight >}}

## View statics

We can use `stats/2` to get the value of the server statistics.

We can use stats/2 to get the value of the server statistics.

{{< highlight erlang >}}
14> game:stats(S2).
{stats,1,21}
15> 
{{< /highlight >}}

During this test with a single player `1` correct and `21` incorrect guesses
where made.

## test/4

Let's try the `test/4` function. 

{{< highlight erlang >}}
15> S2b = game:test(v2, 1, 10, 2).
<0.230.0>
P1 [1 , 10] X =/= 5  
P2 [1 , 10] X =/= 2  
P1 [1 , 10] X =/= 1  
P2 [1 , 10] X =/= 3  
P1 [1 , 10] X =/= 8  
P2 [1 , 10] X =/= 9  
P2 [1 , 10] X =/= 9  
P1 [1 , 10] X =/= 5  
P2 [1 , 10] X =/= 9  
P2 [1 , 10] X =/= 10 
P1 [1 , 10] X =/= 4  
P1 [1 , 10] X =/= 8  
P2 [1 , 10] X =/= 10 
P2 [1 , 10] X =:= 7  (8 guesses, PID = <0.232.0>)
P1 [1 , 10] X =/= 5  
P1 [1 , 10] X =/= 2  
P1 [1 , 10] X =/= 1  
P1 [1 , 10] X =/= 3  
P1 [1 , 10] X =/= 10 
P1 [1 , 10] X =/= 5  
P1 [1 , 10] X =/= 10 
P1 [1 , 10] X =/= 1  
P1 [1 , 10] X =/= 1  
P1 [1 , 10] X =/= 4  
P1 [1 , 10] X =:= 7  (17 guesses, PID = <0.231.0>)
16> 
{{< /highlight >}}

Use the `stats/1` function to get the statistics from this game. 

{{< highlight erlang >}}
16> game:stats(S2b).
{stats,2,23}
17>
{{< /highlight >}}

During this test with two players `2` correct and `23` incorrect guesses where
made.


