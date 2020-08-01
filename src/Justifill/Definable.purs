module Justifill.Definable
  ( class Definable
  , class DefinableFields
  , getFieldsDefined
  , define
  , defined
  , undefined
  , UndefinedOr
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (class RowToList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Row as R
import Type.RowList as RL
import Undefined as Undefined
import Unsafe.Coerce (unsafeCoerce)

foreign import data UndefinedOr :: Type -> Type
undefined :: forall a. UndefinedOr a
undefined = unsafeCoerce Undefined.undefined 
defined :: forall a. a -> UndefinedOr a
defined = unsafeCoerce
foreign import isUndefined :: forall a. UndefinedOr a -> Boolean
instance eqUndefinedOr :: (Eq a) => Eq (UndefinedOr a) where
  eq x y = case isUndefined x, isUndefined y of
    true, true -> true
    false, true -> false
    true, false -> false
    false, false -> ((unsafeCoerce x) :: a) == ((unsafeCoerce y) :: a)
instance showUndefinedOr ::  (Show a) => Show (UndefinedOr a) where
  show x = if isUndefined x then "undefined" else show "(defined " <> show ((unsafeCoerce x) :: a) <> ")"
  
class Definable undefined defined where
  define ∷ undefined -> defined

instance definableRecord ::
  ( RowToList undefined xs
  , DefinableFields xs undefined () defined
  ) =>
  Definable (Record undefined) (Record defined) where
  define x = Builder.build builder {}
    where
    builder ∷ Builder.Builder (Record ()) (Record defined)
    builder = getFieldsDefined (RL.RLProxy ∷ RL.RLProxy xs) x
else instance definableAToUndefinedOr :: Definable a (UndefinedOr a) where
  define = defined
else instance definableA :: Definable a a where
  define x = x

class DefinableFields (xs ∷ RL.RowList) (row ∷ #Type) (from ∷ #Type) (to ∷ #Type) | xs -> row from to where
  getFieldsDefined ∷ RL.RLProxy xs -> Record row -> Builder (Record from) (Record to)

-- Base case, nothing is in the row list
instance definableFieldsNil :: DefinableFields RL.Nil row () () where
  getFieldsDefined _ _ = identity

instance definableFieldsCons ::
  ( IsSymbol name
  , R.Lacks name from'
  , R.Cons name a trash row
  , R.Cons name justA from' to
  , DefinableFields tail row from from'
  , Definable a justA
  ) =>
  DefinableFields (RL.Cons name a tail) row from to where
  getFieldsDefined _ r = first <<< rest
    where
    first = Builder.insert nameP (define val)
    val = Record.get nameP r
    rest = getFieldsDefined tailP r
    nameP = SProxy ∷ SProxy name
    tailP = RL.RLProxy ∷ RL.RLProxy tail
    name = reflectSymbol nameP
