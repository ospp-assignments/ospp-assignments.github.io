---
title: Tic-Tac-Toe 
weight: 100
draft: true
---

<img src="/v1/images/introduction-to-erlang/game/tic-tac-toe.jpg" style="width:400px;"/>

Implement the turned based two player game [Tic-Tac-Toe][wp-tic-tac-toe] using the generic game server. 

[wp-tic-tac-toe]: https://en.wikipedia.org/wiki/Tic-tac-toe


<!--

/Users/karl/teaching/p2p/2014/lab1_by_karl/game

gen_game.erl
ttt.erl

1> G = ttt:start().
<0.80.0>
2> gen_game:call(G, {move, b2, x}).
      1   2   3
    +---|---|---+
  a |   |   |   |
    +---|---|---|
  b |   | x |   |
    +---|---|---+
  c |   |   |   |
    +---|---|---+  turn: o
ok
3> gen_game:call(G, {move, a3, o}).
      1   2   3
    +---|---|---+
  a |   |   | o |
    +---|---|---+
  b |   | x |   |
    +---|---|---+
  c |   |   |   |
    +---|---|---+  turn: x
ok
4> gen_game:call(G, {move, a3, x}).
invalid
5> gen_game:call(G, {move, b1, x}).
      1   2   3
    +---|---|---+
  a |   |   | o |
    +---|---|---+
  b | x | x |   |
    +---|---|---+
  c |   |   |   |
    +---|---|---+  turn: o
ok
6>

TODO: 

Add client.erl 

Make the client read input and print the board

First version: A single client handles both players by making them take turns. 

Second version: Two clients, one for each player on the same machine. .

Third version: Use distributed Erlang to make it possible for the two players to play over the network. 
-->
