# Purescript Justifill

This module provides three interesting functions:
    - `fill` Which adds `undefined` to records with missing fields
    - `define` Which turns non-`UndefinedOr` values into `defined`s
    -  and `justifill` which combines the two

## Examples:
```purescript
filled :: { name :: UndefinedOr String, id :: Int }
filled = fill { id: 10 }
-- { name: Nothing, id: 10 }
```

```purescript
justified :: { name :: UndefinedOr String, age :: UndefinedOr Int }
justified = justify { name: "Mark", age: 40 }
-- { name: Just "Mark", age: Just 40 }
```

```purescript
full :: { name :: UndefinedOr String, age :: UndefinedOr Int, id :: Int }
full = justifill { name: "Mark", id: 12 }
-- { name: Just "Mark", age: Nothing, id: 12 }
```

## Use cases
This can be handy when using APIs with many optional inputs (Props in React Components for example).

It can also be a viable alternative to something like `purescript-options`:

You can type your options as simple records with `UndefinedOr`s for non-mandatory fields:

```purescript
foreign import someJSImpl :: Foreign -> Effect Unit

type SomeJSOptions =
  { option1 :: UndefinedOr Int
  , option2 :: UndefinedOr Number
  -- ...
  , option304 :: UndefinedOr String
  , mandatory :: String }

someJS :: SomeJSOptions -> Effect Unit
someJS = write >>> someJSImpl

-- Usage:
result = someJS (justifill { mandatory: "Hi", option302: "Woah!" })
```
