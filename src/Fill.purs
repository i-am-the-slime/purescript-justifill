module Fill where


import Justifill.Definable (class Definable)
import Justifill.Fillable (class Fillable)
import Unsafe.Coerce (unsafeCoerce)

--| Completes records by wrapping provided optional keys in `Just`
--| and filling out non-provided keys with `Nothing`s
--| Example:
--| x :: { name :: Maybe String, age :: Maybe Int, id :: Int }
--| x = justifill { name: "Mark", id: 12 }
--| -- { name: Just "Mark", age: Nothing, id: 12 }
full ∷
  ∀ from thru to.
  Fillable { | thru } { | to } =>
  Definable { | from } { | thru } =>
  { | from } -> { | to }
full = unsafeCoerce
