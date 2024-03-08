---
title: Advanced statistics (v4)
weight: 80
draft: true
---


The next step is to make the server statistics a little more fine grained. This
version of the server uses the following tagged tuple for the server statistics
`{stats, List}` where List is a list of tuples of the form `{ClientPID, {NumRight,
NumWrong}}`.

## game_loop_v4/2

Look at the function `game_loop_v4/2 in the `game.erl` module.

## Todo

In `game_loop_v4/2`, add replies for wrong guesses just as in `game_loop_v3/2` but
don't forget to use `update_stats_v4/3` instead of `update_stats_v3/2`.

{{% note title="Hint" %}}

Guards can be used in receive patterns.

{{% /note %}}

## Todo

Implement working versions of the `update_stats_v4/3` and
`update_player_stat/3` helper function.

{{% note title="Hint" %}}

The function [proplists:get_value/3][proplists:get_value/3] from
the [proplists][proplists] standard library module might come in handy.

[proplists]: http://erlang.org/doc/man/proplists.html

[proplists:get_value/3]: http://erlang.org/doc/man/proplists.html#get_value-3

{{% /note %}}

## Compile from the Erlang shell

Compile the updated module from the Erlang shell.

{{< highlight erlang >}}
20> c(game).
{ok, game}
21>
{{< /highlight >}}

You will still get a few warnings.

## Test from the Erlang shell

Let's try the new version of the server.

{{< highlight erlang >}}
21> S4 = game:test(v4, 1, 20, 5).
<0.344.0>
P1 [1 , 20] X  <  11
P2 [1 , 20] X  <  13
P4 [1 , 20] X  <  15
P3 [1 , 20] X  <  18
P5 [1 , 20] X  <  3
P1 [1 , 10] X =:= 2  (2 guesses, PID = <0.345.0>)
P5 [1 ,  2] X =:= 2  (2 guesses, PID = <0.349.0>)
P2 [1 , 12] X  >  1
P3 [1 , 17] X  <  9
P4 [1 , 14] X  <  12
P2 [2 , 12] X  <  9
P3 [1 ,  8] X  <  3
P4 [1 , 11] X  <  4
P4 [1 ,  3] X  <  3
P2 [2 ,  8] X  <  5
P3 [1 ,  2] X =:= 2  (4 guesses, PID = <0.347.0>)
P4 [1 ,  2] X  >  1
P2 [2 ,  4] X  <  3
P4 [2 ,  2] X =:= 2  (6 guesses, PID = <0.348.0>)
P2 [2 ,  2] X =:= 2  (6 guesses, PID = <0.346.0>)
22>
{{< /highlight >}}

Use the `stats/1` function to view the game statistics.

{{< highlight erlang >}}
22> game:stats(S4).
{stats,[{<0.345.0>,{1,1}},
{<0.346.0>,{1,5}},
{<0.348.0>,{1,5}},
{<0.347.0>,{1,3}},
{<0.349.0>,{1,1}}]}
23>
{{< /highlight >}}


