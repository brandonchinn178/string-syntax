# string-syntax

[![](https://img.shields.io/github/actions/workflow/status/brandonchinn178/string-syntax/ci.yml?branch=main)](https://github.com/brandonchinn178/string-syntax/actions)
[![](https://img.shields.io/hackage/v/string-syntax)](https://hackage.haskell.org/package/string-syntax)

A preprocessor for enabling string-related syntax features, including:
* Multiline strings ([GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/569))
* String interpolation ([GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/570))
* TODO: raw strings?

## Usage

### Multiline strings

```hs
s :: String
s =
  """
  This is the first line
  This is another line
  """
```

### Interpolated strings

```hs
import Data.String.Syntax.Interpolate

x, y :: Int
(x, y) = (1, 2)

s1 :: String
s1 = s"Result: ${x + y}"

-- works with multiline strings
s2 :: String
s2 =
  s"""
  x: ${x}
  y: ${y}
  Result: ${x + y}
  """
```
