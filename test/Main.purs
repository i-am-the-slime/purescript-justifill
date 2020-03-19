module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Justifill (justifill)
import Justifill.Fillable (class Fillable, fill)
import Justifill.Justifiable (class Justifiable, justify)
import Prim.Row (class Lacks)
import Prim.RowList (class RowToList)
import Record (insert)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  describe "fill" do
    it "fills empty records" do
      fill {} `shouldEqual` {}
      fill {} `shouldEqual` { x: Nothing :: Maybe Int }
      fill {} `shouldEqual` { x: Nothing :: Maybe Int, y: Nothing :: Maybe String }
      fill {} `shouldEqual` { x: Nothing :: Maybe { y :: Int } }
      fill {} `shouldEqual` { x: Nothing :: Maybe (Array Int) }
    it "fills partial records" do
      fill { x: Nothing } `shouldEqual` { x: Nothing :: Maybe Int }
      fill { y: Just "hi", x: Nothing } `shouldEqual` { x: Nothing :: Maybe Int, y: Just "hi" }
    it "leaves complete records" do
      fill { x: Nothing, y: Just "Ho" } `shouldEqual` { x: Nothing :: Maybe Int, y: Just "Ho" }
  describe "justify" do
    it "wraps values in Just" do
      justify 4 `shouldEqual` 4
      justify 4 `shouldEqual` (Just 4)
      justify [4] `shouldEqual` (Just [4])
      justify [] `shouldEqual` ([] :: Array Int)
      justify [] `shouldEqual` (Just ([] :: Array Int))
      justify { x: 4 } `shouldEqual` { x: 4 }
      justify { x: 4 } `shouldEqual` { x: Just 4 }
      justify { x: [1,2] } `shouldEqual` { x: Just ([1,2]) }
    it "works for monads" do
      justify { x: pure 3 } `shouldEqual` { x: Just ([3]) }
      justify { x: pure 3 } `shouldEqual` { x: Just 3 }
      justify { x: pure 3 } `shouldEqual` { x: Right 3 :: Either String Int }
  describe "justifill" do
    it "wraps values in Just and fills records" do
      justifill {} `shouldEqual` { x: Nothing :: Maybe Int }
      justifill { name: "Mark", id: [] :: _ Int } `shouldEqual` { name: Just "Mark", age: Nothing :: Maybe Int, id: [] :: Array Int }
      justifill { x: "Hi", y: Just 4 } `shouldEqual` { x: Just "Hi", y: Just 4, c: Nothing :: (Maybe (Array String)) }
    -- These should all not compile!
    -- it "doesn't work for empty Arrays of the wrong type" do
      -- justifill {x: ([] :: Array String)} `shouldEqual` {x: ([] :: Array Int)}
      -- justifill {x:([] :: Array String)} `shouldEqual` {x:Nothing :: (Maybe (Array Int))}
      -- justifill {x:(Nothing :: Maybe String)} `shouldEqual` {x:Nothing :: (Maybe Int)}
    it "works for records that are already complete" do
      justifill { a: "heinz" } `shouldEqual` { a: Just "heinz" }
      justifill { a: 12 } `shouldEqual` { a: 12, b: Nothing :: (Maybe String)}
      justifill { a: 12, b: Just 4 } `shouldEqual` { a: 12, b: Just 4}
      justifill { a: 12, b: Nothing } `shouldEqual` { a: 12, b: Nothing :: Maybe Int }
    it "works with this weird example" do
      bespoke { a: "Hi" } [1,2,3] `shouldEqual` { a: Just "Hi", kids: [1,2,3]}


type Kids2 r
  = ( kids ∷ Array Int | r )

bespoke ∷
  ∀ to thru from.
  Lacks "kids" from =>
  Justifiable { | Kids2 from } { | Kids2 thru } =>
  Fillable { | Kids2 thru } { | Kids2 to } =>
  -- arguments
  Record from ->
  Array Int ->
  Record (Kids2 to)
bespoke partial kids = props
  where
  props ∷ Record (Kids2 to)
  props = justifill partialWithKids
  partialWithKids ∷ Record (Kids2 from)
  partialWithKids = insert _kids kids partial
  _kids = SProxy ∷ SProxy "kids"
