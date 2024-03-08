---
title: Ping Pong
assignment: higher-grade
points: 4
weight: 100
draft: false
---

<h2 class="subtitle">Optional assignment for grade 4</h2>

<img src="/v1/images/introduction-to-erlang/ping-pong/system-overview.png" style="width:333px;"/>

The phrases "ping pong" and "tick tock" are examples of a linguistic phenomenon
called [ablaut reduplication][ablaut-duplication]. In this assignment you will
implement a server that keeps track of such phrases. For example:

- When a clients sends the atom ping to the server, the server replies with the
  atom pong.
- When receiving the atom tick the server responds with the atom tock.

In this assignment you will learn more about how to design client and server
systems in Erlang with hot code swapping and process supervision.

[ablaut-duplication]: https://www.rd.com/culture/ablaut-reduplication/

## Grading

In addition to the mandatory assignments marked with  {{% assignmentIcon
mandatory %}} in the menu, you must also complete this assignment for grade 4.

## Associative map

The stateful  server uses a associative [map][map] to keep track of Ping Pong pairs. 

[map]: https://erlang.org/doc/man/maps.html

## Hot swapping

Erlang supports hot swapping where the code in an executing system can be
updated without stopping the system. Hot swapping goes by many names, for
example hot code loading and code replacement. Read more:

- [Code replacement][code-replacement] (Erlang Reference Manual)
- [Why only new and old version of a module is allowed][why-two] (Stack overflow)

[code-replacement]: https://erlang.org/doc/reference_manual/code_loading.html#code-replacement

[why-two]: https://stackoverflow.com/questions/49045122/erlang-hot-code-loading

## Process supervision

In Erlang processes can be linked to each other. If two processes are linked to
each other and one of them terminates, the other also terminates.
A process that traps exit will not terminate when a linked process
terminates. Instead a special exit message is sent to the process notifying it
about the termination of the other process. If process A traps exit and links to
process B, process A can act as a supervisor of process B. If B terminates due to an
error, a message is automatically sent to A and A can restart B. Read more:

- [Processes and process supervison][processes] (Erlang Reference Manual)
- [Errors and processes][lyse-errors-and-processes] (Learn you some Erlang)
  
[processes]: https://erlang.org/doc/reference_manual/processes.html
[lyse-errors-and-processes]: https://learnyousomeerlang.com/errors-and-processes


## Files to use

In the `introduction-to-erlang/higher-grade/ping-pong` directory you find the
following Erlang modules: 

- `server.erl`
- `client.erl`

## Generate and view documentation 

In the terminal, navigate to the `introduction-to-erlang/higher-grade/ping-pong` directory.
Use the Makefile to generate and view HTML documentation. 

``` bash session
$ make view_doc
```

This should generate and open HTML document in your web browser. If this does
not work:

 ``` bash session
$ make doc_url 
 ```

Then paste the url in the address bar in your web browser. 

## Read the generated documentation

Read the generated documentation for the `server.erl` and `client.erl` modules. 

## Compile in one terminal

Use make to compile. 

``` bash session
$ make
```

## Start an Erlang shell in a second terminal

Open a second terminal and navigate to the `introduction-to-erlang/higher-grade/ping-pong` directory.
Use make to open an Erlang shell.

``` bash session
$ make repl
```

## Load modules

Load the `server` and `client` modules. 

```erlang
1> l(server).
{module,server}
2> l(client).
{module,client}
3>
```

## Start a stateless and unsupervised server

Start a stateless and unsupervised server. 

``` erlang
4> S = server:start(false, false).
<0.86.0>
5>
```

## Manually send messages to the server

Try to send the atom `hello` to the server. 

```erlang
5> S ! hello.
loop/0: Unknown message: hello
hello
6>
```

The server doesn't handle the `hello` message. 

## Test the client API

Test the `client:ping/2` function from the client API. 

``` erlang
6> client:ping(S, tick).
tick - tock
ok
7> client:ping(S, ding).
loop/0: Unknown message: {ping,ding,<0.83.0>}
ping/2: Timeout
8>
```

When you send a `tick` the server is able to respond with `tock`. But when you
send `ding` the server doesn't know how to reply. 

## Add a server reply

Make the server reply with `dong` when the client sends `ding`. 

Compile with make in the first terminal. Load the `server` module and test the
new server in the second terminal. 

``` erlang
8> f(S), l(server), S = server:start(false, false).
<0.93.0>
9> client:ping(S, ding).
ding - dong
ok
10>
```

## Hot code swapping 

Implement and test hot code swapping. To test if hot code swapping works, make a
change to `server.erl`. Compile using make. From the Erlang shell load the
`server` module and trigger a hot code swapping using `server:update/1`. Now you
should be able to update the server without restarting the server. 


``` erlang
10> client:ping(S, king).
loop/0: Unknown message: {ping,king,<0.79.0>}
ping/2: Timeout
ok
11>
```
The server currently doesn't support the `king` message. Make the server reply
to the `king` message. Compile using make. 

Load the new version from the Erlang shell. 

``` erlang
11> l(server).
{module,server}
12> client:ping(S, king).
loop/0: Unknown message: {ping,king,<0.79.0>}
ping/2: Timeout
ok
13> server:update(S).
```

Loading is not enough. Trigger a hot code swap. 

``` erlang
13> server:update(S).
ok
14> client:ping(S, king).
king - kong
ok
15>
```

After the hot code swap, the server is able to reply to the `king`message.
Without restarting the server, the server now runs the new version of the
`server` module. 

## Implement the complete client API

Implement the complete client API. 

## Implement the supervisor

Make the supervisor restart the server if the server terminates due to an error.
Note that for certain messages, the server process will terminate with reason
`simulated_bug`. Use this, to verify that the supervisor is able to restart
supervised server. 


