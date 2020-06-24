# Lambda calculus

* Open terms - contains free variables
* Closed terms (combinators) - doesn't contain free variables

Name shadowing:

```
\xy.(\xz.x+y)
      ^    ^- outer
      inner
```

Fundamental combinators:

* `S = \f.(\g.(\x.fx(gx))) = \fgx.fx(gx)`
* `K = \x.\y.x`
* `I = \x.x`

```
s f g x = f x (g x)
k x y   = x
i x     = x
```

All closed lambda expressions can be expressed in terms of only
the `S` and `K` combinators.

```
SKK
  = ((\xyz.xz(yz))(\xy.x)(\xy.x))
  = \z.(\xy.x)z((\xy.x)z)
  = \z.(\y.z)(\y.z)
  = \z.z
```

```haskell
type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord, Show)
```

Substitution: `(\x.e)a -> [x/a] e` means replace all free
occurrences of variable `x` in `e` with `a`.

```
[x/a] x     = a
[x/a] y     = y (if x /= y)
[x/a] e e'  = ([x/a] e)([x/a] e')
[x/a] \x.e  = \x.e
[x/a] \y.e  = \y.[x/a] e (if x /= y and y ∉ fv(a))
```
where
`fv(e)` is the set of free variables in `e`

**Alpha-equivalence***: `\x.e =α \y.[x/y] e`

For example: `\xy.xy =α \ab.ab`

**Beta-reduction**: `(\x.a) y ->β [x/y] a`

**Eta-reduction**:

```
\x.fx ->η f          (if x ∉ fv(f))
(\x.fx) e ->β (f e)  (if x ∉ fv(f))
```
In Haskell:

```haskell
\x -> f x ~ f
```

**Eta-expansion** (the opposite of the eta-reduction):

```haskell
s, k, i :: Expr
i = Lam "x" (Var "x")
k = Lam "x" (Lam "y" (Var "x"))
s = Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
```

```
Y (combinator) = \R.(\x.(R(xx))\x.(R(xx)))
```

```
YR
  = \f.(\x.(f(xx))\x.(f(xx)))R
  = (\x.(R(xx))\x.(R(xx)))
  = R(\x.(R(xx))\.x(R(xx)))
  = RYR = RRYR = R…RYR
```

```
Y = SSK(S(K(SS(S(SSK))))K)
```


```
e := x e1 e2 \x.e fix e
```

```
let rec x = e1 in e2
~
let x = fix (\x. e1) in e2
let x = fix (\x. fix (\x. e1) in e2) e1 in e2

fix ν -> ν (fix ν)
```

```haskell
let fact = fix (\fact -> \n ->
  if (n == 0)
  then 1
  else (n * (fact (n-1))));

let rec fib n =
  if (n == 0)
  then 0
  else if (n==1)
  then 1
  else ((fib (n-1)) + (fib (n-2)));
```

```
ω (omega combinator)
\x.xx

Ω = ωω = (\x.xx)(\x.xx)
```
