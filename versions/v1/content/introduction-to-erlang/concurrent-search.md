---
title: Concurrent search
assignment: higher-grade
points: 5
weight: 150
draft: false
---

<h2 class="subtitle">Optional assignment for grade 5</h2>

<img src="/v1/images/introduction-to-erlang/search/system-overview.png" style="width:333px;"/>

In this assignment you will simulate a randomized concurrent search in a huge
search space. This can be seen as an abstraction of systems similar to
 [Folding@home][folding], [SETI@home][seti], and [bitcoin mining][bitcoin]. 

[seti]: https://en.wikipedia.org/wiki/SETI@home
[bitcoin]: https://en.wikipedia.org/wiki/Bitcoin#Mining
[folding]: https://en.wikipedia.org/wiki/Folding@home

## Grading 

In addition to complete the grade 4 [Ping Pong](ping-pong) assignment, you must
also complete this assignment for grade 5.

## System overview

A server keeps a secret number and  number of worker processes competes to find
the secret number. Each worker makes a random guess and asks the server to check
if the guess i correct or not. The first process to find the secret number is
declared the winner and the other threads should stop searching. 
A master process creates the server and a number of worker processes. The
workers updates the master about their progress and the master maintain
statistics about the workers. 

## Common patterns

Erlang systems are usually decomposed into smaller parts using a collection of
processes. Each process type is usually defined in a module with one or ore
`start` functions, a process `looop` and a `stop` function. 

start
: The start function is used to spawn one ore more processes executing the
  recursive `loop` function. 

loop
: The tail recursive `loop` function maintains the process state. The process
  changes states in response to received messages. 
  
stop
: The `stop` function is used to shut down the process. 

## Hide the message passing

The messages passing protocol is usually hidden using a number 
number of exported functions. These functions takes care of sending messages to
the process and waiting for a responses. The message passing protocol then
becomes an internal implementation detail. 

## Files to use

You find all files to use for this assignment in the
`introduction-to-erlang/higher-grade/search` directory. 

## Server

The server is quite simple. Look at the code in `src/server.erl` and play around with server from
the Erlang shell by sending explicit messages to the server process. 

## Worker

A draft version of the worker process can be found in `src/worker.erl`.
A worker process should make random guesses and send them to the server. The
server will reply back telling the worker if the guess was correct or not. 

- The worker should keep guessing until a correct guess is made.
- The worker should keep count of the number of guesses made. 
- The worker must be able to communicate with both the server and the master. 
- For each guess, the worker should notify the master of the current number of
  guesses and whether or not the worker found the secret number. 
- When a worker finds the secret number, all other workers should be terminated.

**Tip:** Make the master keep track of the PIDs of all workers.


## Master

A draft version of the master process can be found in `src/master.erl`.
The master start up the server and all workers. The Master keep track of the
progress of all workers using a associative [map][map]. 

[map]: https://erlang.org/doc/man/maps.html

**Tip:** Use the worker PIDs as keys in the map. 

**Tip:** For each Pid in the map, associate a tuple with the number of guessers made by the worker
and status of the worker (for example, searching, winner or loser). 


## Makefile

You can use the Makefile to compile and start the system. 

``` bash session
$ make test
```

The Makefile can also be used to generate HTML documentation.

``` bash session
$ make doc
```

To view the generated HTML documentation. 

``` bash session
$ make view_doc
```

If the above doesn't work, use: 

``` bash session
$ make doc_url
```

, and paste the URL in the address bar in your web browser. 


## Test each module in isolation

A good strategy is to incrementally implement the functions in a module and test
them manually from the Erlang shell. 

## Example output

In the following example, 10 workers search for a secret number between 1
and 100. 

``` bash session
$ erl -noshell -pa ebin -eval "master:start(10, 1, 100)" -s init stop
<0.78.0>  50
<0.79.0>  48
<0.80.0>  48
<0.81.0>  66
<0.82.0>  14
<0.83.0>  50
<0.84.0>  18
<0.85.0>  71
<0.86.0>  73
<0.87.0>  93
<0.78.0>  62
<0.79.0>  72
<0.80.0>  87
<0.81.0>  70
<0.82.0>  22
<0.83.0> 100
<0.84.0>  16
<0.85.0>  25
<0.86.0>  86
<0.87.0>  27
<0.78.0>  66
<0.79.0>  32
<0.80.0>  81
<0.81.0>  60
<0.82.0>  62
<0.83.0>  26
<0.84.0>  34
<0.85.0>  91
<0.86.0>  10
<0.87.0>  29
<0.78.0>  52 <=== FOUND IT :-)
<0.79.0>  92
<0.80.0>  89
<0.81.0>  42
<0.82.0>  28
<0.83.0>  54
<0.84.0>  88
<0.85.0>  77
<0.86.0>  63
<0.87.0>  40
<0.79.0>  17
<0.80.0> I lose :(
<0.81.0> I lose :(
<0.82.0> I lose :(
<0.83.0> I lose :(
<0.84.0> I lose :(
<0.85.0> I lose :(
<0.86.0> I lose :(
<0.87.0> I lose :(
<0.79.0> I lose :(


Final statistics from the master:

#{<0.78.0> => {4,52,winner},
  <0.79.0> => {5,17,loser},
  <0.80.0> => {4,89,loser},
  <0.81.0> => {4,42,loser},
  <0.82.0> => {4,28,loser},
  <0.83.0> => {4,54,loser},
  <0.84.0> => {4,88,loser},
  <0.85.0> => {4,77,loser},
  <0.86.0> => {4,63,loser},
  <0.87.0> => {4,40,loser}}

$
```

In the final statistics from the master, we see that the process `<0.78.0>`
found the secret number `52` after `4` guesses.
