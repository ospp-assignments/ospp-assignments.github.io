---
title: Concurrent addition
weight: 101
assignment: higher-grade
draft: true
---

<!-- 

Images 

https://docs.google.com/presentation/d/1txTxGN4xd6nhGJ3F8EIxY3QZ-BY5zwLbSlgHSgkptXU/edit#slide=id.gcf2f9861_02 

-->

<h2 class="subtitle">Optional assignment for higher grade</h2>

<img src="/v1/images/introduction-to-erlang/addition/simple-addition.png" style="width: 150px;"/>

## Learning outcomes

To complete this assignment, you must:

- interpret and implement a set of requirements
- make a system design
- define function interfaces and test cases (EUnit)
- integrate all parts into a working system
- provide documentation (EDoc).

## Grade 4

For grade four you must implement requirements 0, 1, 2 and 3.

### Requirement 0 (Erlang)

The system should be implemented in Erlang. 

### Requirement 1 (basic input and output)

This requirement specifies the basic input and output of the system. 

[radix]: https://en.wikipedia.org/wiki/Radix

#### Input

[Radix][radix] - one of binary, hexadecimal or decimal. 

Representations of two
numbers A and B in the given radix.

#### Output

A textual representation of the addition A + B.

#### Example

Given Radix = 10 (decimal), A = 127 and B = 93,  a textual
representation of the addition A + B should look similar to his. 

``` text
   1 1
   - -
   1 2 7
     9 3
 + -----
   2 2 0
```

### Requirement 2 (concurrent addition of segments)

By giving an optional input **N**, the digits in the input arguments **A**
 and **B** should be broken down in **N** segments: **A<sub>N</sub>**,
 **A<sub>N-1</sub>**, ..., **A<sub>1</sub>** and **B<sub>N</sub>**,
 **B<sub>N-1</sub>**, ..., **B<sub>1</sub>** , where **X<sub>N</sub>**
 represents the most significant digits in **X** and **X<sub>1</sub>**
 represents the least significant digits in **X**. The segments should be of
 equal length, i.e., containing approximately the same number of digits.

For any pair of segments **A<sub>i</sub>** and **B<sub>i</sub>**:

<center>
length(**A<sub>i</sub>**) == length(**B<sub>i</sub>**).
</center>

Let **C<sub>i</sub>** be the carry-in and **C<sub>i+1</sub>** be the carry-out when
adding segments **A<sub>i</sub>** and **B<sub>i</sub>**. The addition of the segments
should be done concurrently, hence for each pair of segments:

<center>
{**Sum<sub>i</sub>**, **C<sub>i+1</sub>**} = **A<sub>i</sub>** + **B<sub>i</sub>** + **C<sub>i</sub>**
</center>

, should be calculated by an individual process **P<sub>i</sub>**.
In the following example, two decimal numbers **A** and **B** are split into **N=3** segments.

<img src="/v1/images/introduction-to-erlang/addition/process-structure-1.png"/>

### Requirement 3 (verbose output)

When starting the system, provide an input that enables or disables verbose
output. 
When verbose output is enabled, print out detailed information about critical
steps in the internal processing. 

## Grade 5

For grade 5, in addition to requirements 0, 1, 2 and 3, you must also
implement requirements 4, 5, 6, 7 and 8. 

### Requirement 4 (random sleep)

Because computations in a concurrent system can interact with each other while
they are executing, the number of possible execution paths in the system can be
extremely large.

One way to test more paths is to introduce random sleeps in the system.
Obviously this makes the system slower but we do this while testing the system
hoping to explore more of the execution space.

Within a segment **X** of length **n**, digits are numbered **x<sub>n</sub>**,
**x<sub>n-1</sub>**, ..., **x<sub>1</sub>** where **x<sub>n</sub>** is the most
significant digit and **x<sub>1</sub>** is the least significant digit. The
addition of two digits requires a carry-in **c<sub>i</sub>** and produces a sum
and a carry-out **c<sub>i+1</sub>**.

There should be an option to introduce a random sleep after each digit addition
{**sum<sub>i</sub>**, **c<sub>i+1</sub>**} = **a<sub>i</sub>** +
**b<sub>i</sub>** + **c<sub>i</sub>** within the process **P<sub>i</sub>**. An
optional argument {**Min**, **Max**} defines the minimal and maximal random
sleep time.

### Requirement 5 (speculative execution)

In order to start calculating a segment sum, the carry-in must be known. This is
a sequential bottleneck in the design.

On way to tackle the sequential bottleneck is to calculate segments sums for all
possible values of the carry in. When the real carry in is known, the correct
segment sum can be selected.

There should be an option to turn on speculative execution on the two possible
values **0** and **1** for the the carry-in **C<sub>i</sub>**.

When adding two segments, a process is spawned for each possible value of the
carry-in. When the correct carry-in is available the correct segment sum can be
selected.

<img src="/v1/images/introduction-to-erlang/addition/speculative-execution.png"/>


### Requirement 6 (conditional process termination)

As soon as the correct carry-in for a segment pair is available, there is no need to
speculate any more since we know which of the two speculative processes will
yield the desired result. 

As soon as the correct carry-in **C<sub>s</sub>** for the segment pair
**A<sub>s</sub>** and **B<sub>s</sub>**  is available,
the speculative processes **P<sub>s,x</sub>** where **x** ≠ **C<sub>s</sub>**
should be terminated by the process **P<sub>s<sub>**.

<img src="/v1/images/introduction-to-erlang/addition/conditional-termination.png"/>
### Requirement 7 (automatic testing)

Provide EUnit test cases for all functions with a deterministic relationship
between the inputs and the output. 

### Requirement 8 (documentation)

Provide EDoc documentation for all functions. 

## Higher grade code grading

This assignment will be graded on Monday, Mars 8 at 08:15 - 10:00 in room P1549. 


