---
title: If and case
weight: 6
draft: false
---

In Erlang the if statement is a little different than in most other programming
languages. In short, the if statement can only be used to test the result of
expressions that are guaranteed to have no side effects. On the other hand, the
Erlang case expression can be used to test the result of any expression.

{{% notice class="tip" title="The official Erlang documentation" %}}

The information on this page has been adopted from the official Erlang
documentation about [expressions][doc-expressions] and the subsections about [if][doc-if],
[case][doc-case] and [guard sequences][doc-guard-sequnce]. 

[doc-expressions]: https://erlang.org/doc/reference_manual/expressions.html
[doc-if]: https://erlang.org/doc/reference_manual/expressions.html#if
[doc-case]: https://erlang.org/doc/reference_manual/expressions.html#case
[doc-guard-sequnce]: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences

{{% /notice %}}

[wp-side-effect]: https://en.wikipedia.org/wiki/Side_effect_(computer_science)

## Guards and guard sequences

In order to understand the if statement you must first understand guards and
guard sequences. 

### Guard sequences

A guard sequence is a sequence of guards, separated by
semicolon `;`.

``` erlang
Guard1;...;Guard
```

The guard sequence is `true` if at least one of the guards is `true`. The
remaining guards, if any, are not evaluated.

### Guards

A guard is a sequence of guard expressions, separated by comma `,`.

``` erlang
GuardExpr1,...,GuardExprN
```

The guard is `true` if all guard expressions evaluate to `true`.

The set of valid guard expressions (sometimes called guard tests) is a subset of
the set of valid Erlang expressions. The reason for restricting the set of valid
expressions is that evaluation of a guard expression must be guaranteed to be
free of side effects. Valid guard expressions are the following:

- The atom `true`
- Other constants (terms and bound variables), all regarded as `false`
- Term comparisons
- Arithmetic expressions
- Boolean expressions
- Short-circuit expressions (`andalso`, `orelse`)
- Calls to the the followoing BIFs (Built In Functions):
    - `is_atom/1`
    - `is_binary/1`
    - `is_bitstring/1`
    - `is_boolean/1`
    - `is_float/1`
    - `is_function/1`
    - `is_function/2`
    - `is_integer/1`
    - `is_list/1`
    - `is_map/1`
    - `is_number/1`
    - `is_pid/1`
    - `is_port/1`
    - `is_record/2`
    - `is_record/3`
    - `is_reference/1`
    - `is_tuple/1`

If an arithmetic expression, a Boolean expression, a short-circuit expression,
or a call to a guard BIF fails (because of invalid arguments), the entire guard
fails. If the guard was part of a guard sequence, the next guard in the sequence
(that is, the guard following the next semicolon) is evaluated.

## The if statement

This is the syntax of the if expression.

``` erlang
if
    GuardSeq1 ->
        Body1;
    ...;
    GuardSeqN ->
        BodyN
end
```

The branches of an if-expression are scanned sequentially until a guard sequence
`GuardSeq` that evaluates to `true` is found. Then the corresponding `Body`
(sequence of expressions separated by `,`) is evaluated.

The return value of `Body` is the return value of the if expression.


If no guard sequence is evaluated as `true`, an `if_clause` **run-time error**
occurs. If necessary, the guard expression `true` can be used in the last
branch, as that guard sequence is always `true`.

### Example of a valid if expression 

In the following example, a valid if expression is used to calculate the return
value of the `test_a/1` function.

``` erlang
test_a(X) ->
    if
        is_atom(X) ->
            {atom, X};
        X =:= 33 ->
            {equal, 33};
        X > 10 andalso X < 20 ->
            {between, X};
        true ->
            {true, X}
    end.
```

A few examples of calling the `test_a/1` function with the return value as a
`%%` comment.

``` erlang
test_a(blipp)    %%  {atom,blipp}
test_a(33)       %%  {equal,33}
test_a(15)       %%  {between,15}
test_a(3)        %%  {true,3}
test_a("hello")  %%  {true,"hello"}
```

### Example of an invalid if expression 

In the following example, the user defined function `double/1` is used in one of
the guards in the if expression in the `test_b/1` function.

``` erlang
double(N) ->
    2*N.

test_b(X) ->
    if
        is_atom(X) ->
            {atom, X};
        X =:= 33 ->
            {equal, 33};
        double(X) =:= 66 ->
            {double, X};
        X > 10 andalso X < 20 ->
            {between, X};
        true ->
            {true, X}
    end.
```

User defined functions are not allowed in guards. When compiling, Erlang will
give the following error.


``` erlang
call to local/imported function double/1 is illegal in guard
```


## The case statement

This is the syntax of the case expression.

``` erlang
case Expr of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

The expression `Expr` is evaluated and the patterns Pattern are sequentially
matched against the result. If a match succeeds and the optional guard sequence
`GuardSeq` is `true`, the corresponding `Body` is evaluated.

The return value of `Body` is the return value of the case expression.

If there is no matching pattern with a `true` guard sequence, a `case_clause`
**run-time error** occurs.

### Testing the return value of a user defined function

In the following example, the user defined function `double/1` is used in a
valid case expression in the `test_c/1` function.


``` erlang
double(N) ->
    2*N.

test_c(X) ->
    case double(X) of
        66 ->
            {sixsix, X};
        N when N > 10 ->
            {double_gt_10, X};
        _ ->
            {unknown, X}
    end;
```


A few examples of calling the `test_c/1` function with the return value as a
`%%` comment.

``` erlang
test_c(33)  %%  {sixsix, 33}
test_c(4)   %%  {unknown, 4}
test_c(9)   %%  {double_gt_10, 9}
```

### Matching and deconstruction 

The case expression is often used to match and deconstruct terms. In the
following example a case expression is used to match and deconstruct a list. 

``` erlang
test_d(X) ->
    case X of
        [one|T] -> {one, T};
        _ -> {unknown, X}
    end.
```

A few examples of calling the `test_d/1` function with the return value as a
`%%` comment.

``` erlang
test_d([one, 2, 3])  %%  {one, [2,3]}
test_d([1, 2, 3])    %%  {unknown, [1, 2,3]}
```

### Case is similar to pattern matching in function clause heads

The `test_d/1` function above is equivalent to the `test_e/1` function below. 

``` erlang
test_e([one|T])
    -> {one, T};
test_e(_) -> 
    {unknown, X}.
```

### Case is allowed anywhere an expression is allowed

The advantage of the case expression is that it can be used anywhere an
expression is allowed. In the `foo/2` function below, the local variable `Z` is bound
to the value of the case expression. 

``` erlang
foo(X, Y) ->
    Z = case X of
            [one|T] ->
                {one, T};
            _ -> 
                {unknown, X}
        end,
    bar(Y, Z).
```

