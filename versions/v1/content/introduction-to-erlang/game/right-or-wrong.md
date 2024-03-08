---
title: Right or wrong (v1)
weight: 40
draft: true
---

The next step is for the server to tell the clients whether or not their guesses
are right or wrong. 

## game_server_v1/1

In the file `game.erl` you find the following function declaration with an updated
version of previous game server loop. 


{{< highlight erlang  "linenos=inline" >}}
game_loop_v1(Secret) ->
    receive
        {request, {guess, Secret}, From} ->
            From ! {reply, {right, Secret}},
            game_loop_v1(Secret);
        {request, {guess, N}, From} ->
            From ! {reply, {wrong, N}},
            game_loop_v1(Secret);
        {request, version, From} ->
            From ! {reply, {version, v1}},
            game_loop_v1(Secret)
    end.
{{< /highlight >}}

Note the first receive pattern `{request, {guess, Secret},` From} and how the
variable `Secret` is used both as the argument to the server loop and in this
pattern. When you create a game server, the variable Secret is bound to the
value you provide as argument to `game_loop_v1/1`, hence the pattern `{request,
{guess, Secret}, From}` will only match a correct guess.

An alternative is to use a guarded pattern to detect correct guesses.

{{< highlight erlang >}}
{request, {guess, N}, From} when N == Secret -> 
    From ! {reply, {right, Secret}}, 
    game_loop_v1(Secret);
{{< /highlight >}}

For each incoming message, the receive patterns are tried from top to bottom
until a match is found. Therefore the second receive pattern  `{request, {guess,
N}, From}` will only match incorrect guesses since correct guesses have already
been matched by the first pattern.

The third and last receive pattern makes it possible for clients to ask for the
server version. Here we simply replies with the tuple `{reply, {version, v1}}` to
such a request.

## start/2

In the beginning of `game.erl` you find the following function declaration 

{{< highlight erlang >}}
start(v1, Secret) ->
    spawn(?MODULE, game_loop_v1, [Secret]);
{{< /highlight >}}

Using this functions makes is easy to spawn a new server from the Erlang shell.

{{% highlight erlang %}}
6> S1 = game:start(v1, 55).
<0.62.0>
7> 
{{% /highlight %}}

## guess/2

In `game.erl` you also find the following function which makes it easy to sent a
guess request to the server and wait for the reply.

{{< highlight erlang "linenos=inline">}}
guess(Server, N) ->
    Server ! {request, {guess, N}, self()},
    receive
        {reply, Reply} -> Reply
    end.
{{< /highlight >}}

We can now try and send requests to the server and see the replies.

{{% highlight erlang %}}
7> game:guess(S1, 17).
{wrong,17}
6> game:guess(S1, 55). 
{right,55}
8> 
{{% /highlight %}}

## version/1

There is also a function for asking the server for the version.
{{% highlight erlang %}}
8> game:version(S1).
v1
9> 
{{% /highlight %}}
