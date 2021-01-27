# Exercises from Finding Success in Haskell

# TODO

* add cache to github pipeline
* build on all branches
* publish pages on master only

An implementation of code from [Finding Success (and Failure) in
Haskell](https://leanpub.com/finding-success-in-haskell) by Julie Moronuki and
Chris Martin.

## Pre-requisites

Install with Cabal:

  * HUnit-1.6.0.0

Install with Stack:

  * text-1.2.3.1
  * validation-1

## API Documentation

* [GitHub Pages](https://frankhjung.github.io/haskell-userapp/)
* [GitLab Pages](https://frankhjung1.gitlab.io/haskell-userapp/)
  * [API](https://frankhjung1.gitlab.io/haskell-userapp/html/userapp/index.html)
  * [Test Coverage](https://frankhjung1.gitlab.io/haskell-userapp/hpc/index.html)

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

To coerce a function, for example from:

```haskell
cleanWhitespace :: Rule Text
```

To:

```haskell
cleanWhitespace :: Rule Password
```

Then:

```haskell
(coerce cleanWhitespace :: Rule Password) password
```

Where:
```haskell
type Rule a = (a -> Validation Error a)
```

## References

* https://leanpub.com/finding-success-in-haskell
* https://github.com/matt-noonan/gdp-paper/
