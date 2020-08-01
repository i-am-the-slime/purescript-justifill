module Justifill.Definable
  ( class Definable
  , class DefinableFields
  , getFieldsDefined
  , define
  , defined
  , notAFunction
  , UndefOr
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (class RowToList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Row as R
import Type.RowList as RL
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UndefOr :: Type -> Type
notAFunction :: forall a. UndefOr a
notAFunction = unsafeCoerce undefined
defined :: forall a. a -> UndefOr a
defined = unsafeCoerce
foreign import isUndefined :: forall a. UndefOr a -> Boolean
instance eqUndefOr :: (Eq a) => Eq (UndefOr a) where
  eq x y = case isUndefined x, isUndefined y of
    true, true -> true
    false, true -> false
    true, false -> false
    false, false -> ((unsafeCoerce x) :: a) == ((unsafeCoerce y) :: a)
instance showUndefOr ::  (Show a) => Show (UndefOr a) where
  show x = if isUndefined x then "notAFunction" else show "(defined " <> show ((unsafeCoerce x) :: a) <> ")"
  
class Definable notAFunction defined where
  define ∷ notAFunction -> defined

instance definableRecord ::
  ( RowToList notAFunction xs
  , DefinableFields xs notAFunction () defined
  ) =>
  Definable (Record notAFunction) (Record defined) where
  define x = Builder.build builder {}
    where
    builder ∷ Builder.Builder (Record ()) (Record defined)
    builder = getFieldsDefined (RL.RLProxy ∷ RL.RLProxy xs) x
else instance definableAToUndefOr :: Definable a (UndefOr a) where
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
