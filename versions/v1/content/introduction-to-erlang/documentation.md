---
title: Generate and view documentation
draft: false
weight: 8
---


In the terminal, navigate to the `introduction-to-erlang/mandatory` directory. 

``` bash session
$ cd introduction-to-erlang/mandatory
```

Use make to generate html documentation for the provided Erlang modules.

``` bash session
$ make doc
```

Use the `help` script to view the html documentation in your default
web browser.

``` bash session
$ ./help
```

If the help script doesn't work, you can get the url of the generated html documentation: 

``` bash session
$ make doc_url
```
