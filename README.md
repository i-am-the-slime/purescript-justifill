# Purescript Justifill

This module provides three interesting functions:
    - `fill` Which adds `Nothings` to records with missing fields
    - `justify` Which turns non-`Maybe` values into `Just`s
    -  and `justifill` which combines the two

## Examples:
```purescript
filled :: { name :: Maybe String, id :: Int }
filled = fill { id: 10 }
-- { name: Nothing, id: 10 }
```

```purescript
justified :: { name :: Maybe String, age :: Maybe Int }
justified = justify { name: "Mark", age: 40 }
-- { name: Just "Mark", age: Just 40 }
```

```purescript
justifilled :: { name :: Maybe String, age :: Maybe Int, id :: Int }
justifilled = justifill { name: "Mark", id: 12 }
-- { name: Just "Mark", age: Nothing, id: 12 }
```

## Use cases
This can be handy when using APIs with many optional inputs (Props in React Components for example).

Combined with `simple-json` it can also be a viable alternative to something like `purescript-options`:

You can type your options as simple records with `Maybe`s for non-mandatory fields:

```purescript
foreign import someJSImpl :: Foreign -> Effect Unit

type SomeJSOptions =
  { option1 :: Maybe Int
  , option2 :: Maybe Number
  -- ...
  , option304 :: Maybe String
  , mandatory :: String }

someJS :: SomeJSOptions -> Effect Unit
someJS = write >>> someJSImpl

-- Usage:
result = someJS (justifill { mandatory: "Hi", option302: "Woah!" })
```