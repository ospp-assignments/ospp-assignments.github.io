---
title: Generate documentation with EDoc
weight: 30
draft: true
---

EDoc is the Erlang program documentation generator. It is now time to use EDoc
to generate HTML documentation from the provided source code.

## Create a new directory for the generated documentation 

In the terminal, make sure you are in the assignment root directory. 

``` shell
$> ls
game.beam     game.erl
```

In the directory listing make sure you see `game.erl`. You may also see
`game.beam` if you previously have compiled `game.erl`.


Create a new director named `doc`. 

``` shell
$> mkdir doc
```

Use the `ls -F` command to verify that the new `doc` directory was created. 

``` shell
$> ls -F
doc/         game.beam      game.erl
```

Make sure you see the newly created `doc` directory in the directory listing. 

{{% note title="ls -F" %}}

When listing contents of a directory using the `ls` command, the flag `-F`
appends an extra character at the end of pathname for certain types of
directory entries, for example: 

- a slash `/`  after a directory
- an asterisk `*` after an executable
- an at sign `@` after a symbolic link

{{% /note %}}

## Generate HTML documentation 

From the Erlang shell, use `Edoc` to generate HTML files in the new `doc` directory. 

{{% highlight erlang %}}
5> edoc:files(["game.erl"], [{dir, doc}]).
ok
6>
{{% /highlight %}}

This will create various files including `game.html` in the `doc` directory. 

``` shell
$> ls -1 doc
edoc-info
erlang.png
game.html
index.html
modules-frame.html
overview-summary.html
packages-frame.html
stylesheet.css
```

## View the generated documentation 

Open the file `game.html` file in a web browser and use as a reference as you keep
working. 

## Learn more about EDoc

To learn more more about EDoc the following resources may be useful.

- [User's guide][edoc-users-guide]
- [Reference manual][edoc-reference-manual]

[edoc-users-guide]: http://erlang.org/doc/apps/edoc/chapter.html
[edoc-reference-manual]: http://erlang.org/doc/man/edoc.html
