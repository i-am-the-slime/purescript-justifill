module Justifill.Justifiable
  ( class Justifiable
  , class JustifiableBackwards
  , class JustifiableFields
  , getFieldsJustified
  , justify
  , justifyBackwards
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList (class RowToList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Equality (class TypeEquals)
import Type.Row as R
import Type.RowList as RL

class Justifiable unjust just where
  justify ∷ unjust -> just

instance justifiableRecord ::
  ( RowToList unjust xs
  , JustifiableFields xs unjust () just
  ) =>
  Justifiable (Record unjust) (Record just) where
  justify x = Builder.build builder {}
    where
    builder ∷ Builder.Builder (Record ()) (Record just)
    builder = getFieldsJustified (RL.RLProxy ∷ RL.RLProxy xs) x
else instance justifiableAToMaybe :: Justifiable a (Maybe a) where
  justify = Just
else instance justifiableA :: Justifiable a a where
  justify = identity
--| In case of empty Arrays or Nothing values, it helps inference to go from the
--| add a functional dependency from the output to the input type
--| However, that messess with the more basic cases (going from a -> a fails to find an instance)
--| Therefore, we only fall back to this at this point
--| Note how b and a are unrelated types here
else instance justifiableBackwards :: JustifiableBackwards b a => Justifiable b a where
  justify = justifyBackwards

class JustifiableBackwards unjust just | just -> unjust where
  justifyBackwards :: unjust -> just

instance justifyBackwardsMaybeArray :: JustifiableBackwards (Array a) (Maybe (Array a))  where
  justifyBackwards = Just
else
instance justifyBackwardsMaybeArray2 :: JustifiableBackwards (Array a) (Array a)  where
  justifyBackwards = identity
else
instance justifyBackwardsHmm :: JustifiableBackwards (m a) (m a) where
  justifyBackwards = identity 

class JustifiableFields (xs ∷ RL.RowList) (row ∷ #Type) (from ∷ #Type) (to ∷ #Type) | xs -> row from to where
  getFieldsJustified ∷ RL.RLProxy xs -> Record row -> Builder (Record from) (Record to)

-- Base case, nothing is in the row list
instance justifiableFieldsNil :: JustifiableFields RL.Nil row () () where
  getFieldsJustified _ _ = identity

instance justifiableFieldsCons ::
  ( IsSymbol name
  , R.Lacks name from'
  , R.Cons name a trash row
  , R.Cons name justA from' to
  , JustifiableFields tail row from from'
  , Justifiable a justA
  ) =>
  JustifiableFields (RL.Cons name a tail) row from to where
  getFieldsJustified _ r = first <<< rest
    where
    first = Builder.insert nameP (justify val)
    val = Record.get nameP r
    rest = getFieldsJustified tailP r
    nameP = SProxy ∷ SProxy name
    tailP = RL.RLProxy ∷ RL.RLProxy tail
    name = reflectSymbol nameP
