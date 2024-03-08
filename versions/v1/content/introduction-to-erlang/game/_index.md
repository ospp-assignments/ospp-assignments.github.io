---
title: Client-server game
assignment: higher-grade
weight: 1000
draft: true
---

<h2 class="subtitle">Optional assignment for higher grade</h2>


<!-- 
TODO: Manual import from P2P 2014 in [Ping Pong][pp].

[pp]: https://pingpong.uu.se/courseId/8598/content.do?id=29188487

-->

In this assignment you will construct a simple [client-server][wp-client-server]
game where the server stores a secret number and the clients tries to guess the
secret number originally only known by the server.

[wp-client-server]: https://en.wikipedia.org/wiki/Client%E2%80%93server_model

{{% figure 
     src="/v1/images/introduction-to-erlang/game/client-server-overview.png"
     maxwidth="500px"
     caption="Client-server architecture for a simple game where the server keeps a secret number which clients tries to guess."
%}}

The server stores a secret number. The clients tries to guess the secret number
originally only known by the server. The server and clients communicate by
sending messages to each other. When a client sends a guess to the server, the
server will answer with a result. If the guess was wrong the client will try to
guess again.  

