---
title: FIFO and TDD
assignment: mandatory
weight: 20
draft: false
---

<h2 class="subtitle">Mandatory assignment</h2>

In a first-in, first-out
([FIFO](https://en.wikipedia.org/wiki/FIFO_(computing_and_electronics))) queue,
elements are consumed from the queue in the same order
as they arrive to the queue.
[Test-driven development](https://en.wikipedia.org/wiki/Test-driven_development)
(TDD) is a software development process that relies on the repetition of a very
short development cycle where the developer:

1. writes one or more failing automated test cases that defines a desired improvement or
   new functionality.
2. writes code to implement the desired improvement or new functionality.
3. runs the automated tests to verify the implementation.
4. Finally refactors the new code to acceptable standards.

You will now use TDD to implement a FIFO queue in Erlang.

## Single linked lists and time complexity

Erlang uses [immutable][wp-immutable] single linked lists. The [time complexity][wp-big-o] of
adding a new head to a list is [O(1)][wp-o-constant] and so is the time
complexity of removing the head of a list. Adding a new element to the end of a
list is [O(n)][wp-o-linear] where n is the number of elements in the list.

[wp-immutable]: https://en.wikipedia.org/wiki/Immutable_object

[wp-big-o]: https://en.wikipedia.org/wiki/Big_O_notation

[wp-o-constant]: https://en.wikipedia.org/wiki/Time_complexity#Constant_time

[wp-o-linear]: https://en.wikipedia.org/wiki/Time_complexity#Linear_time

## An immutable FIFO

How can we implement an immutable FIFO?

### Using a single linked list

If a single linked lists is used as FIFO and push is implemented by adding a new
head to the list, push becomes an O(1) operation. On the other hand, for pop we now need to
find and take away the last element in the list. Pop is now an O(n) operation where n
is the number of elements in the list.

An alternative is to implement push by appending a new element at the end of the
list. Push now becomes an O(n) operation. On the other hand, pop will be
O(1) since we can simply take out the head of the list.

For both of the above alternatives either push or pop becomes O(n).

### Using two single linked lists

What if we use two list for the FIFO?

- One list `In` for push where we add new elements to the head of the list O(1).
- One list `Out` for pop where we take out elements from the head of the list O(1).

How can this be accomplished? 

Init
: Initially both `In` and `Out` are empty. 

Push
: When pushing we simply add a new head to `In`. Push is an O(1) operation.

Pop
: When popping and `Out` is empty, the element to pop is at the end of `In`.
  Replace `Out` with the reverse of `In`, an O(n) operation. Now the element to
  pop is the head of the reversed list and can be taken out in O(1). 

Push is always a O(1) operation. Pop sometimes is an O(n)
operation and sometimes an O(1) operation depending on whether the `Out` list
currently is empty or not. 

### In and Out lists step by step

To better understand how a FIFO with two lists `In` and `Out` works, study the
following example. 

<img src="/v1/images/introduction-to-erlang/fifo/step-by-step.png" style="width: 555px;"/>

## Example usage of an immutable FIFO

You will implement a functional and [immutable][wp-immutable] FIFO buffer
[abstract data type][wp-adt]. In Erlang, all values are immutable. Functions
updating the FIFO must return a new updated FIFO. In the below example program,
a new variable is used to store a new immutable version of the
FIFO after each update to the FIFO.

[wp-immutable]: https://en.wikipedia.org/wiki/Immutable_object
[wp-adt]: https://en.wikipedia.org/wiki/Abstract_data_typ

``` erlang
F1 = fifo:new(),
F2 = fifo:push(F1, 77),
F3 = fifo:push(F2, 3),
io:format("size(F3): ~w~n", [fifo:size(F3)]),
{X, F4} = fifo:pop(F3),
io:format("X = ~w~n", [X]),
io:format("empty(F4): ~p~n", [fifo:empty(F4)]),
{Y, F5} = fifo:pop(F4),
io:format("Y = ~w~n", [Y]),
io:format("empty(F5): ~p~n", [fifo:empty(F5)]).
```

Running the above example program should result in the following output:

``` erlang
size(F3): 2
X = 77
empty(F4): false
Y = 3
empty(F5): true
```


## EUnit

Erlang comes with a [unit test](https://en.wikipedia.org/wiki/Unit_testing)
framework named [EUnit](http://erlang.org/doc/apps/eunit/chapter.html).

## File to use

In `introduction-to-erlang/mandatory/src/fifo.erl` you find the skeleton code for a FIFO module.


## Test driven development (TDD)

In the true spirit of TDD, the tests should be written before the
implementation. Therefore you are provided with a number of EUnit test cases.
The test cases captures design decisions.

## First compile

In the terminal, navigate to the `introduction-to-erlang/mandatory` directory.

``` bash session
$ cd introduction-to-erlang/mandatory
```

To make sure you trigger a new compile, delete the following beam file.

``` bash session
$ rm ebin/fifo.beam
```
{{% notice title="Note" %}}

You don't have to delete the beam file before each compile in the future. The
make utility will automatically recompile if you made any changes to
`fifo.erl`.
{{% /notice %}}

Use make to compile the `src/fifo.erl` module and create the resulting `ebin/fifo.beam` file.

``` bash session
$ make
```

You will see a number of warnings:

``` bash session
src/fifo.erl:26: Warning: variable 'X' is unused
src/fifo.erl:35: Warning: variable 'In' is unused
src/fifo.erl:37: Warning: variable 'H' is unused
src/fifo.erl:37: Warning: variable 'In' is unused
src/fifo.erl:37: Warning: variable 'T' is unused
```

Warnings - but success! The warnings about unused variables will go away one after another as you work your way through the assignment.


If you get the following warning,

``` bash session 
Warning: opaque type fifo() is not exported
```

, export the opaque type `fifo()`like this.

``` Erlang
-opaque fifo()::{fifo, list(), list()}.
-export_type([fifo/0]).
```

## EUnit

From the Linux shell, run EUnit in verbose mode:

``` bash session
$ make testv_fifo
```

EUnit will now run all the test cases and report the results.

``` bash session
======================== EUnit ========================
module 'fifo'
 fifo:60: new_test_...ok
 fifo:61: new_test_...ok
 fifo:62: new_test_...ok
 fifo: push_test...ok
 fifo: push_pop_test...*failed*
in function fifo:pop/1 (src/fifo.erl, line 41)
  called as pop(tbi)
  in call from fifo:'-push_pop_test/0-fun-0-'/0 (src/fifo.erl, line 84)
  **error:function_clause
    output:<<"">>

    undefined
    *** test generator fifo:size_test_/0 failed ***
    **in function fifo:push/2 (src/fifo.erl, line 32)
      called as push(tbi,bar)
      in call from fifo:f1/0 (src/fifo.erl, line 88)
      in call from fifo:size_test_/0 (src/fifo.erl, line 91)
      **error:function_clause


      =======================================================
        Failed: 1.  Skipped: 0.  Passed: 4.
        One or more tests were cancelled.

```

From the EUnit report we see that four tests passed with status `ok`.
The first test that failed was the `push_pop_test()` which terminated with reason:
`::function_clause`. This means that no matching function clause is found when
evaluating a function call.
Read [here](http://erlang.org/doc/reference_manual/errors.html) for more
information about the various exit reasons.


## Dig deeper

To investigate this further we will test the `fifo` module manually from the
Erlang shell:

``` erlang
erlang> F1 = fifo:new().
{fifo,[],[]}
```

A new fifo was created. The new fifo is represented by a 3-tuple where the first
element is the atom fifo and the second and third elements are empty lists.
Now, lets try to push a value to the new fifo:

``` erlang
erlang> F2 = fifo:push(F1, a).
tbi
```

Aha! When pushing the atom `a` to the fifo `F1`, the atom `tbi` is returned. What
happens if we use the value of `F2` in a pop:

``` erlang
erlang> F3 = fifo:pop(F2).
** exception error: no function clause matching fifo:pop(tbi) (src/fifo.erl,
line 33)
```

When we try to pop, we call `fifo:pop(tbi)` and there is no function clause
matching. In other words, the function `fifo:pop/1` is not defined when called
with the atom `tbi` as argument. The problem is that `fifo:push/2` returns the atom
`tbi`.

{{% notice style="warning" title="Todo" %}}

Change the implementation of `fifo:push/2` according to the comments in the
source.

{{% /notice %}}

## Make all tests pass

Your task is to make all test pass by making necessary changes to the
implementation of `fifo:pop/1` and `fifo:push/2`.

{{% notice style="tip" title="Small steps" %}}

It is often recommended to make a few small changes, then compile and run
the tests again.
{{% /notice %}}

{{% notice style="tip" title="Additional tests from the Erlang shell" %}}

In addition to the EUnit test cases, it is often very helpful to test
the functions in a module manually from the Erlang shell.

{{% /notice %}}

## EDoc

[EDoc](http://erlang.org/doc/apps/edoc/chapter.html) lets you write the
documentation of an Erlang program as comments in the source code itself, using
tags on the form `@Name ...`. A source file does not have to contain tags for EDoc
to generate its documentation, but without tags the result will only contain the
basic available information that can be extracted from the module.

{{% notice style="warning" title="Todo" %}}

Provide proper descriptions for all `@doc` tags.

{{% /notice %}}

Generate new html doc files:

``` bash session
$ make doc
```

Now you can reload the web page and see the new information added to the html
documentation page for `fifo.erl`. To open the documentation from scratch:

``` bash session
$ ./help
```


u may also use the Makefile to print the url to the documentation index page to
the terminal:

``` bash session
$ make doc_url
```

## Type specifications

Erlang is a dynamically typed language. Still, it comes with a notation for
declaring sets of Erlang terms to form a particular type, effectively forming a
specific sub-type of the set of all Erlang terms. EDoc will detect any type
declarations and include this information in the generated documentation.

Here you can read
more
[how to declare types and function specifications](http://erlang.org/doc/reference_manual/typespec.html) using
EDoc.


{{% notice style="warning" title="Todo" %}}

Use `-spec` type declarations to add type declarations to `fifo:push/2` and
`fifo:pop/1`.

{{% /notice %}}

{{% notice style="tip" title="Hints" %}}

Look at the type declarations provided for `fifo:new/0`, `fifo:empty/1` and
 `fifo:size/1`. You may also look at the type declarations for the functions in
 `tutorial.erl`.

 {{% /notice %}}
