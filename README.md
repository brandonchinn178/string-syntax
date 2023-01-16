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
import qualified Data.String.Syntax.Interpolate as StringSyntax

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

## Troubleshooting

### `Ambiguous type variable arising from a use of interpolatePrec` + `Overlapping instances for StringSyntax.InterpolateValue a0 String`

This can happen if you nest an interpolated string inside of another, since the compiler is unable to infer the type of the inner interpolation:

```
interpolatePrec 0 (interpolatePrec 0 x)
                   ^^^^^^^^^^^^^^^^^^^
```

To fix this, simply add an explicit type annotation:

```
s"Interpolated: ${s"This is a nested interpolation: ${x}" :: String}
```
