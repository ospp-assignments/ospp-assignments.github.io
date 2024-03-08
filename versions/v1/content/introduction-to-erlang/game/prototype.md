---
title: Prototype (v0)
weight: 20
draft: true
---

In the file `game.erl` you find the following function declaration witih a draft
outline for the game server loop.

{{< highlight erlang  "linenos=inline" >}}
game_loop_v0(Secret) ->
    receive
        {request, {guess, Guess}, From} ->

            %% Send reply back to client.
            From ! {reply, {undefined, Guess}},

            %% Recursive call to wait for next request.
            game_loop_v0(Secret)
    end.
{{< /highlight >}}

The secret number is held as the argument to the recursive `game_loop_v0/1`
function.


## Tagged tuples 

In the game loop the receive construct is used to wait for a message from one of
the clients. Here we wait for a message matching the following pattern.

{{< highlight erlang >}}
{request, {guess, Guess}, From}
{{< /highlight >}}

We could choose any pattern we want but we choose to use this **nested tuple**. The
first element of the tuple is the **atom** `request`. 

{{% note class="tip" title="Tagged tuples" %}}
Any tuple where the first
element is an atom is called called a tagged tuple. It is considered good
practice to use tagged tuples as messages in Erlang. The tag can be seen as a
short descripton of the intended meaning of the message. Using taged tuples as
messages also makes it easy to pattern match incomming messages based on the
tag. 
{{% /note %}}

Here the tag `request` makes it easy to see that this tuple contains
information about a client request. 

The second element of the tuple is also a tagged tuple, namely `{guess, Guess}`.
Here the **atom** `guess` is used to describe what kind of request we have received,
in this case a guess from one of the clients. 

## Pattern matching 

When an incoming `{request, {guess, Guess}, From}` message from a client is
matched, the variables `Guess` and `From` will be bound to the values in the
incoming message. `Guess` will be the number guessed by the client and `From`
will be the process identifier (PID) of the client making the request.

## Testing from the Erlang shell

Open a terminal and make sure the file `game.erl` is in the current working
directory.

``` shell
> ls
game.erl  
>
```

Open the Erlang shell.

``` shell
beurling> erl
```

Now the Erlang shell should start. 

{{% highlight erlang %}}
Erlang R15B03
(erts-5.9.3)
[source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.3 (abort with ^G)
1> 
{{% /highlight %}}

Compile the `game` module. 

{{% highlight erlang %}}
1> c(game).
game.erl:314: Warning: function update_player_stat/3 is unused
game.erl:314: Warning: variable 'Event' is unused
game.erl:314: Warning: variable 'PlayerPID' is unused
game.erl:314: Warning: variable 'PlayerStats' is unused
game.erl:323: Warning: variable 'Event' is unused
game.erl:323: Warning: variable 'PlayerPID' is unused
{ok,game}
2> 
{{% /highlight  %}}

Note that there are a few warnings. These warnings will go away one after one as
you make changes to the code. For now we ignore these warnings. 
Spawn a new game server.

{{% highlight erlang %}}
2> S0 = spawn(game, game_loop_v0, [55]).
<0.43.0>
3> 
{{% /highlight %}}

Here the number `55`  is used as the secret number. The result of `spanw/3` is for a
new process to be created. Here the PID of the new server is `<0.43.0>`.
We can now try and send a request to the server.

{{% highlight erlang %}}
3> S0 ! {request, {guess, 42}, self()}. 
{request,{guess,42},<0.31.0>}
4> 
{{% /highlight %}}

Here we guess that the secret number is `42` and we use `self()` to get the PID of
the Erlang shell.

Hopefully the game server was able to receive our message and send a reply back.
But where is the reply sent? The reply is sent back to the mailbox of the Erlang
shell. We can use `flush()` to empty the mail box of the shell.

{{% highlight erlang %}}
4> flush(). 
Shell got {reply,{undefined,42}}
ok
5> 
{{% /highlight %}}

Aha, the shell received the tuple `{reply, {undefined, 42}}` in the mail box.

## Conclusions 

The first prototype makes it possible to start a server storing  a
secret number. The server can also respond to guesses from clients. 

A limitation of the first prototype server is that the server will always reply with
`{reply, {undefined, X}}` when a client sends a `{request, {guess, X}, ClientPID}` tuple
to the server, the server doesn't let the clients know whether their guesses are
correct or not.
