## Notes

Note for Russian-speaking readers:

To understand more about type inference algorithm
I recommend watching these 2 lectures:

* https://www.lektorium.tv/lecture/13185
* https://www.lektorium.tv/lecture/13208

## Substitution in polytype.

`apply(s, Forall as t):`


```haskell
forall a b c
   . (a -> c)
  -> d
  -> (e -> Bool)
  -> b

-- s:
a -> Bool
c -> (Bool -> Int)
d -> (Bool -> Bool)
e -> Int

-- s':
d -> (Bool -> Bool)
e -> Int

forall a b c
   . (a -> c)
  -> (Bool -> Bool)
  -> (Int -> Bool)
  -> b
```

`FTV(t):`

```haskell
t = forall a b c d. (a -> b) -> (p -> q) -> c -> d
ftv(t) = [p, q]
```

## Generalization example.

Type env:

`G :=`

```haskell
f :: forall a. a -> Bool
g :: forall c. c : Bool -> Int
```

```
ftv(G) = [a, c]
```

Lets generalize an example function:

```haskell
h :: (c -> a) -> d -> b -> e

ftv(h) = [c, a, d, b, e];
ftv(G) = [a, c];
ftv(h) - ftv(G) = [d, b, e];

generalize(h) = forall d b e. (c -> e) -> d -> b -> e
```

We can’t generalize `a` and `c` because they’re bound (present
in our context / type environment).
