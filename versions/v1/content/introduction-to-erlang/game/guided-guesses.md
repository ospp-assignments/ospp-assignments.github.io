---
title: Guided guesses (v3)
weight: 70
draft: true
---

The next step is to make the server give the clients a hint whether the guessed
number is greater or less than the secret number. 

## game_loop_v3/2

In the file `game.erl`, take a look at the `game_loop_v3/2`  function. 

### Todo

You must add code to `game_loop_v3/2` so that the tuples `{reply, {wrong, N, gt}}`
and `{reply, {wrong, N, lt}}` are sent as replies when the guess `N` is greater
respectively smaller than the secret number.

{{% note title="Hint" %}}

Guards can be added to receive patterns similar to [guards added to
patterns in a case expression][lyse-case-of].

[lyse-case-of]: https://learnyousomeerlang.com/syntax-in-functions#in-case-of
{{% /note %}}

Until now, the players made guesses by picking a random number between `Min` and
`Max`. Since the server now tells a player if the guessed number was to large or
to small a player should use this information in order to increase the
probability of guessing the right number. 

### Todo

You must make changes in `player_loop/5` so that a player takes advantage of the
added information about an incorrect guesses. 

{{% note title="Hint" %}}

How can the interval `[Min, Max]` used by a player to make a random guess be
reduced based on the reply from the server?

{{% /note %}}

## Compile from the Erlang shell

Compile the updated module from the Erlang shell.

{{< highlight erlang >}}
17> c(game).
{ok, game}
18>
{{< /highlight >}}

You might still get a few warnings.

### Test from the Erlang shell

Let's try the new version of the server using a larger interval for the secret
number.

{{< highlight erlang >}}
18> S3 = game:test(v3, 1, 100, 2).
<0.289.0>
P1 [1  , 100] X  >  51  
P2 [1  , 100] X  >  48  
P1 [52 , 100] X  >  54  
P1 [55 , 100] X  <  82  
P2 [49 , 100] X  <  58  
P2 [49 ,  57] X  >  50  
P1 [55 ,  81] X  <  80  
P2 [51 ,  57] X  >  53  
P1 [55 ,  79] X  <  74  
P2 [54 ,  57] X  >  56  
P1 [55 ,  73] X  <  73  
P2 [57 ,  57] X =:= 57  (6 guesses, PID = <0.291.0>)
P1 [55 ,  72] X  <  64  
P1 [55 ,  63] X  <  63  
P1 [55 ,  62] X  >  56  
P1 [57 ,  62] X  <  61  
P1 [57 ,  60] X  <  60  
P1 [57 ,  59] X  <  58  
P1 [57 ,  57] X =:= 57  (13 guesses, PID = <0.290.0>)
19> 
{{< /highlight >}}

We can use `stats/2` to get the value of the server statistics.

{{< highlight erlang >}}
19> game:stats(S3).
{stats,2,17}
20> 
{{< /highlight >}}

Despite a large initial interval both players was able to find the secret
number much faster than with the previous version of the game server. 
