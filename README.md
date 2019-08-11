# Exercises from Finding Success in Haskell

An implementation of code from [Finding Success (and Failure) in
Haskell](https://leanpub.com/finding-success-in-haskell) by Julie Moronuki and
Chris Martin.

Install with Cabal:

  * HUnit-1.6.0.0

Install with Stack setup

  * text-1.2.3.1
  * validation-1

## Coerce

Here are some notes on use of
[Data.Coerce](https://hackage.haskell.org/package/base/docs/Data-Coerce.html).

See also [GHC Coercible](https://wiki.haskell.org/GHC/Coercible).

### a value

To coerce a value, for example from `Username` to `Text`:

```haskell
Success (User u _) -> Data.Text.IO.putStrLn (Data.Text.concat ["Welcome ", coerce u])
```

### a function

To coerce a function, for example from

```haskell
cleanWhitespace :: Rule Text
```

to

```haskell
cleanWhitespace :: Rule Password
```

then:

```haskell
(coerce cleanWhitespace :: Rule Password) password
```

Where:
```haskell
type Rule a = (a -> Validation Error a)
```

