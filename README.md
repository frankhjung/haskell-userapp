# Exercises from Finding Success in Haskell

An implementation of code from [Finding Success (and Failure) in
Haskell](https://leanpub.com/finding-success-in-haskell) by Julie Moronuki and
Chris Martin.

## Quick Start

To install dependencies

- HUnit (use Cabal to install)
- text
- validation

To build, test and run call:

```bash
make setup default
```

### Note

`validation` is not part of the stanard LTS release so must explicitly be in
`stack.yaml`.

## GHCi

To load source and test files to
[GHCi](https://docs.haskellstack.org/en/latest/ghci/#ghci) include the
[HUnit](https://wiki.haskell.org/HUnit_1.0_User%27s_Guide) package:

```bash
stack ghci userapp
```

Then load other libraries as modules:

```haskell
λ> :m + Data.Text Data.Validation Data.Coerce Data.Char
```

## API Documentation

- [GitHub Pages](https://frankhjung.github.io/haskell-userapp/)
- [GitLab Pages](https://frankhjung1.gitlab.io/haskell-userapp/)

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

To coerce a function, for example:

From:

```haskell
cleanWhitespace :: Rule Text
```

To

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

- <https://leanpub.com/finding-success-in-haskell>
- <https://github.com/matt-noonan/gdp-paper/>
