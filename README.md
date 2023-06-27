### What is this for?

Hopefully this will help with intuitively understanding do-notation.

See `src/Main.purs`, which has examples and explanations.

There are 6 functions in there (ex1 to ex6) that are all logically equivalent and only differ in syntax.

The first (ex1) is the most naive, and each subsequent function gradually improves on the syntax until we reach do-notation (ex6).

### Some examples

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
