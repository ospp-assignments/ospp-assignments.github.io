---
title: Clone repository
assignment: github
weight: 7
draft: false
---

Before you continue, you must clone the [introduction-to-erlang][repo]
repository.

[repo]: https://github.com/ospp-assignments/introduction-to-erlang

## Use the git command

From the terminal, navigate to a directory where you want the cloned directory
to be created and execute the following command.

``` bash session 
$ git clone https://github.com/ospp-assignments/introduction-to-erlang
```

Now you should see something similar to this in the terminal.

``` bash session
Cloning into 'introduction-to-erlang'...
remote: Enumerating objects: 48, done.
remote: Counting objects: 100% (48/48), done.
remote: Compressing objects: 100% (37/37), done.
remote: Total 48 (delta 4), reused 48 (delta 4), pack-reused 0
Unpacking objects: 100% (48/48), done.
```

## Use the tree command

To get an overview of the cloned repository, use the `tree -d` command.

``` bash session
$ tree -d introduction-to-erlang
```

Now you should see a tree view of the directory strucure.

``` bash session
introduction-to-erlang
├── README.md
├── higher-grade
│   ├── README.md
│   ├── ping-pong
│   │   ├── Makefile
│   │   ├── doc
│   │   │   ├── erlang.png
│   │   │   ├── my_style.css
│   │   │   ├── overview.edoc
│   │   │   └── uu.png
│   │   ├── ebin
│   │   ├── src
│   │   │   ├── client.erl
│   │   │   └── server.erl
│   │   └── view_doc
│   └── search
│       ├── Makefile
│       ├── doc
│       │   ├── erlang.png
│       │   ├── my_style.css
│       │   └── overview.edoc
│       ├── ebin
│       ├── src
│       │   ├── master.erl
│       │   ├── server.erl
│       │   ├── utils.erl
│       │   └── worker.erl
│       └── view_doc
└── mandatory
    ├── Makefile
    ├── doc
    │   ├── erlang.png
    │   ├── my_style.css
    │   ├── overview.edoc
    │   └── uu.png
    ├── ebin
    ├── help
    └── src
        ├── bang.erl
        ├── death.erl
        ├── fifo.erl
        ├── link.erl
        ├── sfifo.erl
        ├── state.erl
        ├── swap.erl
        └── tutorial.erl

13 directories, 33 files
```
