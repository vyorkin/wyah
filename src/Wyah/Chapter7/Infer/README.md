Note for Russian-speaking readers:

To understand more about type inference algorithm
I recommend watching these 2 lectures:

* https://www.lektorium.tv/lecture/13185
* https://www.lektorium.tv/lecture/13208

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

Generalization example:

Type env:

```haskell
a : Int
c : Bool -> Int
```

function:

```haskell
f :: (a -> b) -> c -> Int
f :: forall b. (a -> b) -> c -> Int
```
