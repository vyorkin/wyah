# Let

Let bindings are semantically equivalent to
applied lambda expressions.

```
let a = e in b := (\a. b) e
```

Toplevel expressions will be written as `let` statements
without a body to indicate that they are added to the global
scope:

```
let S f g x = f x (g x);
let K x y = x;
let I x = x;

let skk = S K K;
```

For now the _evaluation rule_ for `let` is identical to
that of an applied lambda, compare:

```
(\x. e) v       -> [x/v]e   (E-Lam)
let x = v in e  -> [x/v]e   (E-Let)
``**

In later variations of the lambda calculus `let** expressions
will have a different semantics and will differ from applied
lambda expressions.

**Everything can be a lambda term**.
