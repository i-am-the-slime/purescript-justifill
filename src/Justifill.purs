module Justifill where

import Prelude
import Justifill.Fillable (class Fillable, fill)
import Justifill.Justifiable (class Justifiable, justify)

--| Completes records by wrapping provided optional keys in `Just`
--| and filling out non-provided keys with `Nothing`s
--| Example:
--| x :: { name :: Maybe String, age :: Maybe Int, id :: Int }
--| x = justifill { name: "Mark", id: 12 }
--| -- { name: Just "Mark", age: Nothing, id: 12 }
justifill ∷
  ∀ from thru to.
  Fillable { | thru } { | to } =>
  Justifiable { | from } { | thru } =>
  { | from } -> { | to }
justifill = (fill ∷ { | thru } -> { | to }) <<< (justify ∷ { | from } -> { | thru })
