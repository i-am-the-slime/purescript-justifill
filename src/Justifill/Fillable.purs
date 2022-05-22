module Justifill.Fillable
  ( class Fillable
  , class FillableFields
  , fill
  , getFillableFields
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Type.Row as R
import Type.RowList as RL

class Fillable partial complete where
  fill ∷ partial -> complete

class FillableFields (xs ∷ RL.RowList Type) (from ∷ Row Type) (to ∷ Row Type) | xs -> from to where
  getFillableFields
    ∷ Proxy xs
    -> Builder (Record from) (Record to)

instance fillableRecord ::
  ( RowToList missing missingList
  , FillableFields missingList () missing
  , Union partial missing complete
  , Nub complete complete
  ) =>
  Fillable (Record partial) (Record complete) where
  fill o = Builder.build ((Builder.disjointUnion o) <<< (getFillableFields missingListP)) {}
    where
    missingListP = Proxy ∷ _ missingList

instance fillableFieldsNil :: FillableFields RL.Nil () () where
  getFillableFields _ = identity

instance fillableFieldsCons ::
  ( IsSymbol name
  , FillableFields tail from from'
  , R.Lacks name from'
  , R.Cons name (Maybe ty) from' to
  ) =>
  FillableFields (RL.Cons name (Maybe ty) tail) from to where
  getFillableFields _ = first <<< rest
    where
    first = Builder.insert nameP Nothing
    rest = getFillableFields tailP
    nameP = Proxy ∷ Proxy name
    tailP = Proxy ∷ Proxy tail
