module Justifill where

import Prelude
import Justifill.Fillable (class Fillable, fill)
import Justifill.Justifiable (class Justifiable, justify)
import Type.Proxy (Proxy)

--| Completes records by wrapping provided optional keys in `Just`
--| and filling out non-provided keys with `Nothing`s
--| Example:
--| x :: { name :: Maybe String, age :: Maybe Int, id :: Int }
--| x = justifill { name: "Mark", id: 12 }
--| -- { name: Just "Mark", age: Nothing, id: 12 }
justifill
  ∷ ∀ from via to
   . Fillable { | via } { | to }
  => Justifiable { | from } { | via }
  => { | from }
  -> { | to }
justifill = (fill ∷ { | via } -> { | to }) <<< (justify ∷ { | from } -> { | via })

--| A version of justifill that helps type inference along
--| by receiving a `Proxy` of the `thru` type
--| This is useful if you write a wrapper around justifill because
--| it allows to write simpler type annotations
justifillVia
  ∷ ∀ from via to
   . Fillable { | via } { | to }
  => Justifiable { | from } { | via }
  => Proxy { | via }
  -> { | from }
  -> { | to }
justifillVia _ = (fill ∷ { | via } -> { | to }) <<< (justify ∷ { | from } -> { | via })
