# Exercises from Finding Success in Haskell

An implementation of code from [Finding Success (and Failure) in
Haskell](https://leanpub.com/finding-success-in-haskell) by Julie Moronuki and
Chris Martin.

Install with Cabal:

  * HUnit

Install with Stack setup

  * text
  * validation

## API Documentation

* [GitHub Pages](https://frankhjung.github.io/haskell-userapp/)
* [GitLab Pages](https://frankhjung1.gitlab.io/haskell-userapp/)

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


## References

* https://leanpub.com/finding-success-in-haskell
* https://github.com/matt-noonan/gdp-paper/
