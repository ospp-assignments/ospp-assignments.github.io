---
title: Automatic testing
weight: 50
draft: true
---

So far we have spawned game servers and sent client request manually from the Erlang
shell. To make it easier to test the game server we will now automate the
creation of the server and sending of client request to the server.

## player_loop/5

In the file `game.erl`, take a look at the `player_loop/5`  function. This functions
simulates a player by creating a new process that keeps sending random guess
requests to the server until the player stumbles upon the correct number.

A player keeps track of the number of guesses made.
After sending a request to the server, a case expression is used to analyze the
reply from the server.

For now, only look at the two first patterns `{right, N}` and `{wrong, N}` in the
case expression.

The first pattern `{right, N}` is used to print a message similar to this:

{{< highlight erlang >}}
P1 [1 , 10] X =:= 7 (10 guesses, PID = <0.197.0>)
{{< /highlight >}}

, to the terminal when the correct number have been guessed. Here we see that
player number `1` guessed that the secret number `X` is `7` and that this guess was
correct, `X =:= 7`.  We also see the interval in which the player has been making
random guesses `[1, 10]`, the total number of guesses needed 10 and the process id
(PID) of the player process `<0.197.0>`.

The second pattern `{wrong, N}` is used to print out a message similar to this:

{{< highlight erlang >}}
P1 [1 , 10] X =/= 4
{{< /highlight >}}

, to the terminal when an incorrect guess was made. Here we see that player
number `1` guessed that the secret number `X` was `4` by generating a random number in
the interval `[1, 10]` and that this guess was wrong, `X =/= 4.`

### Todo

You must make a small change in the recursive call `player_loop(Server, Min, Max,
Guesses, Meta)` in all the case clauses in order for the player to keep track of
the number of guesses made. 

## test/4

In the file `game.erl`, take a look at the `test/4` function. Calling `test(Version,
Min, Max, NumPlayers)` will generate a random secret number in the interval `Min`
to `Max` (inclusive) and start a game server of the desired `Version`. Next
`NumPlayers` player process will be started.

## Test from the Erlang shell

Let's try the `test/4` function. 

{{< highlight erlang >}}
10> game:test(v1, 1, 10, 1).
<0.196.0>
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 5 
P1 [1 , 10] X =/= 9 
P1 [1 , 10] X =/= 5 
P1 [1 , 10] X =/= 1 
P1 [1 , 10] X =/= 3 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 3 
P1 [1 , 10] X =/= 9 
P1 [1 , 10] X =:= 7 (10 guesses, PID = <0.197.0>)
11> 
{{< /highlight >}}


Here we started a version 1 (`v1`) server with a random secret number between `1` and `10`.
A single player client process `P1` was also started. Player `P1` knows that the
secret number must be between en `1` and `10` and needs `10` attempts to guess the
correct number `7`. Note that the player may guess the same wrong number multiple
times, as in the above example where `3`, `4`, `5`, and `9` where guessed twice. 

Let's try with two player processes.


{{< highlight erlang >}}
11> game:test(v1, 1, 10, 2). 
<0.199.0>
P1 [1 , 10] X =/= 3 
P2 [1 , 10] X =/= 3 
P2 [1 , 10] X =/= 10 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 8 
P2 [1 , 10] X =/= 6 
P2 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 8 
P2 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 7 
P1 [1 , 10] X =/= 1 
P2 [1 , 10] X =/= 1 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 7 
P2 [1 , 10] X =/= 2 
P1 [1 , 10] X =/= 2 
P2 [1 , 10] X =/= 7 
P2 [1 , 10] X =/= 6 
P1 [1 , 10] X =/= 4 
P1 [1 , 10] X =/= 7 
P2 [1 , 10] X =/= 1 
P1 [1 , 10] X =/= 7 
P2 [1 , 10] X =/= 10 
P1 [1 , 10] X =/= 6 
P2 [1 , 10] X =/= 8 
P1 [1 , 10] X =/= 10 
P2 [1 , 10] X =/= 10 
P2 [1 , 10] X =:= 5 (14 guesses, PID = <0.201.0>)
P1 [1 , 10] X =/= 9 
P1 [1 , 10] X =/= 3 
P1 [1 , 10] X =:= 5 (17 guesses, PID = <0.200.0>)
12> 
{{< /highlight >}}

Player `P2` found the secret number `5` after `14` guesses and player `P1` after `17`
guesses.
