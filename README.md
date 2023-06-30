# What is this for?

Hopefully this will help with intuitively understanding do-notation.

See `src/Main.purs`, which has examples and explanations.

### Some relevant links

- https://github.com/purescript/purescript-maybe/blob/v6.0.0/src/Data/Maybe.purs#L129
- https://www.youtube.com/watch?v=gHiyzctYqZ0&t=725s
- https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Control.Bind
- https://stackoverflow.com/questions/8019670/desugaring-do-notation-for-monads
- https://en.wikibooks.org/wiki/Haskell/do_notation

### Some examples of do-notation de-sugaring

#### Ex1
```
do
    f
```
becomes
```
f
```

#### Ex2
```
do
    x <- f
    log x
```
becomes
```
f >>= (\x -> log x)
```

#### Ex3
```
do
    x <- f
    y <- g
    log $ x + y
```
becomes
```
f >>= (\x -> g >>= (\y -> log $ x + y))
```
or equivalently
```
f >>= (\x ->
g >>= (\y ->
log $ x + y))
```
or equivalently
```
f >>= \x ->
g >>= \y ->
log $ x + y
```

#### Ex4
```
do
    x <- f
    y <- g x
    log $ x + y
```
becomes
```
f >>= (\x -> g x >>= (\y -> log $ x + y))
```
