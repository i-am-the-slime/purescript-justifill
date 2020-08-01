module Justifill.Fillable
  ( class Fillable
  , class FillableFields
  , fill
  , getFillableFields
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Justifill.Definable (UndefOr, notAFunction)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Row as R
import Type.RowList as RL

class Fillable partial complete where
  fill ∷ partial -> complete

class FillableFields (xs ∷ RL.RowList) (from ∷ #Type) (to ∷ #Type) | xs -> from to where
  getFillableFields ∷
    RL.RLProxy xs ->
    Builder (Record from) (Record to)

instance fillableRecord ::
  ( RowToList missing missingList
  , FillableFields missingList () missing
  , Union partial missing complete
  , Nub complete complete
  ) =>
  Fillable (Record partial) (Record complete) where
  fill o = Builder.build ((Builder.disjointUnion o) <<< (getFillableFields missingListP)) {}
    where
    missingListP = RL.RLProxy ∷ _ missingList

instance fillableFieldsNil :: FillableFields RL.Nil () () where
  getFillableFields _ = identity

instance fillableFieldsCons ::
  ( IsSymbol name
  , FillableFields tail from from'
  , R.Lacks name from'
  , R.Cons name (UndefOr ty) from' to
  ) =>
  FillableFields (RL.Cons name (UndefOr ty) tail) from to where
  getFillableFields _ = first <<< rest
    where
    first = Builder.insert nameP notAFunction
    rest = getFillableFields tailP
    nameP = SProxy ∷ SProxy name
    tailP = RL.RLProxy ∷ RL.RLProxy tail
    name = reflectSymbol nameP
