---
title: Generic game server (gen)
weight: 90
draft: true
---

All the previous versions of the game server share a lot of logic. It is good
engineering practice to recognize repeated patterns and try to find generic
solutions that can be used to construct specific solutions.

The essence of all the previous game servers can be summarized by the following
function.


{{< highlight erlang >}}
gen_game_loop(State, Callback) ->
    receive
        {request, Request, From} ->
            {reply, Response, NewState} = Callback(Request, From, State),
            From !  {reply, Response},
            gen_game_loop(NewState, Callback)
    end.
{{< /highlight >}}

The function `gen_game_loop/2` takes the following two arguments:

1. `State` - the state of the server.
2. A `Callback` function of arity 3. Erlang is a functional language and we can pass
functions as arguments to functions.

## Separating the generic from the specific

This server is abstract and the `gen_game_loop/2` function only implements the
following generic logic of a game server loop:

1. Receive a client `request`.
2. Construct a `Response`.
3. Construct a `NewState`.
4. Send the `Response` as a `reply` back to the requesting client.
5. Make a recursive call replacing the old `State` with `NewState`.

## callback_v4/3

The following callback function is all that is needed to re-implement version 4
of the game servers together with generic game server.


{{< highlight erlang >}}
callback_v4(version, _From, {Version, _, _} = State) ->
    {reply, {version, Version}, State};

callback_v4(stats, _From, {_, _, Stats} = State) ->
    {reply, {stats, Stats}, State};

callback_v4({guess, Guess}, From, {Version, Secret, Stats}) ->
    Reply = case compare(Guess, Secret) of
                eq -> {right, Guess};
                lt -> {wrong, Guess, lt};
                gt -> {wrong, Guess, gt}
            end,
    Event = element(1, Reply),
    NewState = {Version, Secret, update_stats_v4(From, Event, Stats)},
    {reply, Reply, NewState}.
{{< /highlight >}}


## start/2

To start a generic server we must supply a callback function.

{{< highlight erlang >}}
start(gen, Secret) ->
    spawn(?MODULE, gen_game_loop,
          [{gen, Secret, init_stats_v4()}, fun callback_v4/3]).

{{< /highlight >}}

Try to understand how `callback_v4/3` works together with `gen_game_loop/2`.

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
22>G = game:test(gen, 1, 20, 5).
<0.352.0>
P5 [1 , 20] X  >  7
P1 [1 , 20] X  <  20
P2 [1 , 20] X  >  1
P1 [1 , 19] X  <  16
P3 [1 , 20] X  >  3
P4 [1 , 20] X  <  19
P3 [4 , 20] X  >  11
P5 [8 , 20] X  >  8
P4 [1 , 18] X  >  11
P4 [12, 18] X =:= 12 (3 guesses, PID = <0.356.0>)
P2 [2 , 20] X  <  18
P1 [1 , 15] X  >  11
P3 [12, 20] X  <  13
P5 [9 , 20] X  <  13
P2 [2 , 17] X  >  10
P1 [12, 15] X  <  15
P5 [9 , 12] X =:= 12 (4 guesses, PID = <0.357.0>)
P2 [11, 17] X =:= 12 (4 guesses, PID = <0.354.0>)
P3 [12, 12] X =:= 12 (4 guesses, PID = <0.355.0>)
P1 [12, 14] X  <  13
P1 [12, 12] X =:= 12 (6 guesses, PID = <0.353.0>)
23>
{{< /highlight >}}

Use the `stats/1` function to view the game statistics.

{{< highlight erlang >}}
23> game:stats(G).               
{stats,[{<0.357.0>,{1,3}},
{<0.353.0>,{1,5}},
{<0.354.0>,{1,3}},
{<0.355.0>,{1,3}},
{<0.356.0>,{1,2}}]}
24> 
{{< /highlight >}}

{{< highlight erlang >}}
{{< /highlight >}}

{{< highlight erlang >}}
{{< /highlight >}}
