# string-syntax

Prototype for String Interpolation ([proposal](https://github.com/ghc-proposals/ghc-proposals/pull/570)). Not meant for production usage, but can be used to try out different implementations.

Run the examples!

```bash
# `cabal run` also works
stack run implicit-builder
stack run explicit
stack run implicit-only-string
stack run implicit-no-builder
stack run extensible-th
stack run extensible-hasclass
```

## Usage

In your `cabal.project` file:

```cabal
source-repository-package
  type: git
  location: https://github.com/brandonchinn178/string-syntax
  tag: main
```

In your `.cabal` file:

```cabal
ghc-options: -F -pgmF string-syntax -optF <mode>
build-tool-depends: string-syntax:string-syntax
build-depends: string-syntax
```

See below for available modes.

### Limitations

The following are limitations of the prototype. These will be resolved when implementing in GHC, but implementing these would be more work than it's worth.

* Interpolated expressions cannot include the closing brace character (`}`)
    * This means that nested interpolated expressions are not supported
* Interpolated expressions must be valid Haskell
* String gaps aren't supported
* Errors don't show the correct line, since lines may be shifted in the preprocessor

## Add a new mode

To add a new mode, open a PR with the following changes:

1. New mode added to `allModes`
1. Add an example in `examples/`
1. Add a new section below under "Available modes"
1. Update the "Run the examples" code block above

## Available modes

### `implicit-builder`

This mode will desugar interpolated strings as:

```haskell
-- Original
s"a ${x} b"

-- Desugared
fromBuilder . mconcat $
  [ toBuilder (fromString "a ")
  , interpolate x
  , toBuilder (fromString " b")
  ]
```

This mode uses the following definitions, which are exported from `Data.String.Syntax.ImplicitBuilder` (would be exported from `Data.String.Interpolate.Experimental` if the proposal is accepted).

```haskell
class Buildable s where
  type Builder s = b | b -> s
  toBuilder :: s -> Builder s
  fromBuilder :: Builder s -> s

class Interpolate a s where
  interpolate :: a -> Builder s
```

### `explicit`

This mode will desugar interpolated strings as:

```haskell
-- Original
s"a ${x} b"

-- Desugared
mconcat
  [ fromString "a "
  , x
  , fromString " b"
  ]
```

This mode is "explicit" because it forces the user to explicitly convert everything to the String-like type.

### `implicit-only-string`

This mode will desugar interpolated strings as:

```haskell
-- Original
s"a ${x} b"

-- Desugared
mconcat
  [ fromString "a "
  , fromString (interpolate x)
  , fromString " b"
  ]
```

This mode uses the following definition, which are exported from `Data.String.Syntax.ImplicitOnlyString` (would be exported from `Data.String.Interpolate.Experimental` if the proposal is accepted).

```haskell
class Interpolate a where
  interpolate :: a -> String
```

This mode forgoes the complexity in `implicit-builder` with a multi-param `Interpolate` class by forcing all interpolations to go through `String`. It opts for simplicity at the cost of unsupporting use-cases like auto-escaping SQL queries.

### `implicit-no-builder`

This mode will desugar interpolated strings as:

```haskell
-- Original
s"a ${x} b"

-- Desugared
mconcat
  [ fromString "a "
  , interpolate x
  , fromString " b"
  ]
```

This mode uses the following definitions, which are exported from `Data.String.Syntax.ImplicitNoBuilder` (would be exported from `Data.String.Interpolate.Experimental` if the proposal is accepted).

```haskell
class Interpolate a s where
  interpolate :: a -> s
```

It is similar to `implicit-builder` in power but should lead to less and simpler error messages. The presence of builder and code equivalent to `from Builder . toBuilder` can generate ambiguous type errors. Those errors disappear if there is no builder.

If a builder needs to be used, it can be so by creating `IsString` and `Interpolate` instances for the builder type.

### `extensible-th`

This mode behaves the same as `implicit-only-string`, except it would allow any possibly-qualified identifier as a delimiter (of which `s` is just one provided out of the box).

The given identifier must be a function with the type `[Either String (Q Exp)] -> Q Exp`.

```haskell
-- Original
s"a ${x} b"

-- Desugared
$(Data.String.Interpolate.s
  [ Left "a "
  , Right [| x |]
  , Left " b"
  ])

-- Original
sql"SELECT * FROM user WHERE name = ${name}"

-- Desugared
$(sql
  [ Left "SELECT * FROM user WHERE name = "
  , Right [| name |]
  ])
```

This mode uses the following definitions, which are exported from `Data.String.Syntax.ExtensibleTH` (would be exported from `Data.String.Interpolate.Experimental` if the proposal is accepted).

```haskell
class Interpolate a where
  interpolate :: a -> String
```

If this mode is accepted, `-XStringInterpolation` would imply `-XTemplateHaskell`. The benefit of this mode is string interpolation can run at compile-time, which also makes this mode the most powerful/general of all the options. Indeed, every other mode could be implemented in terms of `extensible-th`.

### `extensible-hasclass`

This mode behaves the same as `implicit-only-string`, except it would allow any possibly-qualified identifier as a delimiter (of which `s` is just one provided out of the box).

The given identifier must be a function with the type `[Either String (HasClass c)] -> a`.

```haskell
-- Original
s"a ${x} b"

-- Desugared
--   s :: [Either String (HasClass Interpolate)] -> String
Data.String.Interpolate.s
  [ Left "a "
  , Right (HasClass x)
  , Left " b"
  ]

-- Original
sql"SELECT * FROM user WHERE name = ${name}"

-- Desugared
--   sql :: [Either String (HasClass ToSql)] -> SqlQuery
sql
  [ Left "SELECT * FROM user WHERE name = "
  , Right (HasClass name)
  ]
```

This mode uses the following definitions, which are exported from `Data.String.Syntax.ExtensibleHasClass` (would be exported from `Data.String.Interpolate.Experimental` if the proposal is accepted).

```haskell
class Interpolate a where
  interpolate :: a -> String

data HasClass c = forall a. c a => HasClass a
```

This mode is similar to `extensible-th`, but trades power for simplicity, providing string interpolation without including any baggage from Template Haskell.
