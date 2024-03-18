---
title: Sequential Erlang
assignment: mandatory
weight: 10
draft: false
---

<h2 class="subtitle">Mandatory assignment</h2>

##  Scope

In Erlang, functions are defined in modules. For a function defined in one
module to be accessible from the Erlang shell or from other modules, the
function must be exported. 

### Function definition

Lets say the function `double/1` is defined in a module with the file name
`test.erl`.

``` erlang
-module(test).

double(A) -> 2*A.
```

### Local use 

Within module `test.erl`, all functions are accessible directly. When defining the
function `four/0` in `test.erl` the function `double/1` can be called directly.

``` Erlang
-module(test).

double(A) -> 2*A.

four() -> double(2).
```

### Export functions

To export one or more functions from a module, the following module attribute
must be placed at the top of the module, but after the `-module(Name)` attribute. 

``` erlang
-export([Name1/Arity1, ..., NameN/ArityN]).
```

This is how you export the `double/1` function from the `test.erl` module. 

``` Erlang
-module(test).
-export([double/1]).

double(A) -> 2*A.

four() -> double(2).
```


### External use

Only exported functions can be called from the Erlang shell or from other
modules. To call a function defined in another module the following syntax must be
used.

``` erlang
Module:Function(Arguments).
```

For example, this is how you call `double/1` defined in the `test.erl` module
form the Erlang shell or from another module.

``` erlang
test:double(2).
```


## File to use

Open the file `introduction-to-erlang/mandatory/src/tutorial.erl` in a text editor.

Emacs is the de facto standard editor for Erlang and provides syntax
highlighting and automatic indentation and is highly recommended but similar
support is provided by many other code editors. 

## First compile

In the terminal, navigate to the `introduction-to-erlang/mandatory` directory and use `make` to compile
all modules. 

``` bash session
$ cd introduction-to-erlang/mandatory
$ make
```

You may now see a number of warnings about unused variables. This is expected
and the warnings will disappear after you completed all the steps in this
tutorial and the other exercises. 
 
## Read the module html documentation

You should already have [generated html documentation](documentation) for all
the provided Erlang modules.

From the documentation menu, chose the tutorial module. On this page you find
documentation for all the exported functions in the `tutorial.erl` module.
Briefly read through the documentation to familiarize yourself with exported
functions and their expected behaviors. 

## The Erlang shell 

From the terminal, navigate to the `introduction-to-erlang/mandatory/src` directory. 

``` bash session
$ cd introduction-to-erlang/mandatory/src
```

This is the directory for all the module source files.  Now, start a new Erlang
shell.

``` bash
$ erl
```

You should see something similar to:

``` erlang
Erlang R15B03
(erts-5.9.3)
[source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]
Eshell V5.9.3 (abort with ^G)
1>
```

Note that the Erlang shell prompt now is `1> `. Add two numbers. 

``` erlang
1> 1 + 3.
4
2>
```

After the result `4` is printed, the new `2>` prompt is printed. The Erlang shell
prompt counts the number of evaluated expressions. 


{{% notice style="tip" title="The Erlang shell prompt" %}}
In all remaining instructions `erlang> ` will be used to denote the Erlang shell
prompt. 
{{% /notice %}}


## Compile from the Erlang shell

If you compile using the Makefile, the new versions of the compiled modules (the
beam files) will not be automatically available in any existing Erlang shells.
If you are interesting in more details, read
more [here](http://erlang.org/doc/man/code.html). When testing individual
modules manually from the Erlang shell it is more convenient to compile from the
Erlang shell. 


You now can compile individual modules directly from the Erlang shell using the
`c/1` function:

``` erlang
erlang> c(tutorial).
```

After a number of warnings about unused variables you should get the following
ok message. 


``` erlang
{ok,tutorial}
erlang>
```

## Test function from the Erlang shell

Execute the `hello/0` function, don't forget the trailing ling period `.`:

``` erlang
erlang> tutorial:hello().
```

The `hello/0` function simply prints `Hello!`.

``` erlang
Hello!
ok
```

## Quit the Erlang shell

To quit the Erlang shell, type `q().` and press enter. 

``` erlang
erlang> q().
```

You should now get back the Linux terminal prompt.

## Beam files

From the Linux terminal:

``` bash session
$ ls | grep tutorial
tutorial.beam
tutorial.erl
```

{{% notice title="Note" %}}
When compiling form the Erlang shell, the resulting beam file is saved in
the current working directory. 
{{% /notice %}}

From the Linux terminal, delete all beam files in the `module-8/mandatory/src` directory: 

``` bash session
$ make clean
```

## Start the Erlang shell


In the Linux terminal, from the `module-8/mandatory/src` directory, start the Erlang
shell. 

``` bash session
$ erl
```

## Recursion

Erlang is a functional language and [recursion](http://learnyousomeerlang.com/recursion) is the fundamental concept used to
achieve loop-like constructs. Have a look at the `tutorial:hello/1` function. What
do you think will happen when we call `tutorial:hello(4)`?

{{% notice style="warning" title="Todo" %}}

Make a dry run of `tutorial:hello(4)` on a piece of paper.

{{% /notice %}}

From the Erlang terminal:

``` erlang
erlang> tutorial:hello(4).
```

Did your pen and paper dry run match? Next, look at the `tutorial:fac/1` function.
This is the straight forward recursive implementation of the [factorial](https://en.wikipedia.org/wiki/Factorial) function:

``` erlang
erlang> tutorial:fac(7).
```

This will return `7!`, i.e., `5040`.

## EDoc

{{% notice style="warning" title="Todo" %}}

Add a proper EDoc description of `tutorial:hello/1` by changing the text
after the `@doc` tag. 

{{% /notice %}}

Update the generated html documentation:

``` bash session
$ make doc
```

Refresh your web browser and make sure you can see the new description of
`tutorial:hello/1`.

## To be implemented

In the tutorial module, the atom `tbi` (short for To Be Implemented) is used as a
place holder. You must  replace all the `tbi` atoms with Erlang code to get working
solutions. 


## Tail recursion

{{% notice style="warning" title="Todo" %}}

Write a [tail recursive](http://learnyousomeerlang.com/recursion#length-tail-recursion) version of the factorial function by completing the
`tutorial:fac_tr/2` function.

{{% /notice %}}


Verify your solution by compiling from the Linux terminal  and testing from the
Erlang shell.

## List comprehensions

{{% notice style="warning" title="Todo" %}}

In the `tutorial:right_triangle/1` function, use a [list comprehension](http://learnyousomeerlang.com/starting-out-for-real#list-comprehensions) to return a
 list of tuples `{A, B, C}` such that `A` and `B` are sides in a right triangle with
 hypotenuse `C`, where `A`, `B`, `C` <= `N`.
 
{{% /notice %}}
 
By calling the `tutorial:simpsons/0` function, a list of members of the Simpson
family is returned.  We want to filter this list to obtain new lists.

{{% notice style="warning" title="Todo" %}}

In the tutorial module, use list comprehensions to complete the implementations
of `simpsons/1`.

{{% /notice %}}

For example, to get a list of all all the names in the family: 

``` erlang
erlang> tutorial:simpsons(names).
["Bart","Snowball II","Homer","Lisa", "Santa's Little Helper","Marge","Spider Pig"]
```

For example, to get a list of all the pets: 

``` erlang
erlang> tutorial:simpsons(pets).
["Snowball II","Santa's Little Helper","Spider Pig"] 
```

## Guarded functions

Function heads can
have [guards](http://learnyousomeerlang.com/syntax-in-functions#guards-guards).
This is a convenient way to handle various cases when defining a function. To
get the [ASCII](https://en.wikipedia.org/wiki/ASCII) value of a character, simply prefix the character with $. For
example, to get the ASCII value of the character A from the Erlang shell: 

``` erlang
erlang> $A.
65
```

{{% notice style="warning" title="Todo" %}}

Complete the `char_to_upper/1` and `char_to_lower/1` functions. Use guarded function
heads to check if the argument character is within range.

{{% /notice %}}

Test your solution: 

``` erlang
erlang> tutorial:char_to_upper($b).
66
```

Note that `char_to_upper/1` takes a ASCII value as argument and returns a ASCII
value. In the above example, `$b` = ASCII(b) = `98` is used as argument and `66` =
ASCII(B) is returned. 

## Mapping functions

Strings in Erlang are simply list of ASCII values, for example:

``` erlang
[$E, $r, $l, $a, $n, $g] = [69, 114, 108, 97, 110, 103] = "Erlang"
```


{{% notice style="warning" title="Todo" %}}

Complete the `str_to_upper/1` and `str_to_lower/1` functions by using the
`lists:map/2` function together with the `char_to_upper/1` and `char_to_lower/1`
functions.

{{% /notice %}}

{{% notice style="tip" title="List comprehensions" %}}
You could also use [list comprehensions](https://www.erlang.org/doc/programming_examples/list_comprehensions) to implement `str_to_upper/1` and
`str_to_lower/1`. 
{{% /notice %}}

## Fold

A common pattern is to recombine the elements of a data structure to form a new
data structure, often this is referred to as [folding](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) a data structure into a new
data structure. One example of a fold is when the elements of a list is
recombined into a single value. The Erlang  `lists:foldl/3` function does exactly
this. 


{{% notice style="warning" title="Todo" %}}

- Complete the `max/1` function by binding `F` to an anonymous function.
- Complete the `count/2` function by binding `F` to an anonymous function.
- Complete the `odd_and_even/1` function by replacing the atom `tbi` in the
  anonymous functions bound to the variable `F`.
{{% /notice %}}
