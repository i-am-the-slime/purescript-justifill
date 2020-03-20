module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Justifill (justifill)
import Justifill.Fillable (fill)
import Justifill.Justifiable (justify)
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
      justify "hi" `shouldEqual` (Just "hi")
      justify [4] `shouldEqual` (Just [4])
      -- Back to using type annotations
      justify ([] :: Array Int) `shouldEqual` ([] :: Array Int)
      justify ([] :: _ Int) `shouldEqual` (Just ([] :: Array Int))
      justify { x: 4 } `shouldEqual` { x: 4 }
      justify { x: 4 } `shouldEqual` { x: Just 4 }
      justify { x: [1,2] } `shouldEqual` { x: Just ([1,2]) }
      justify { x: [1,2] } `shouldEqual` { x: Just ([1,2]) }
    -- it "works for monads" do
      -- justify { x: pure 3 } `shouldEqual` { x: Just ([3]) }
      -- justify { x: Just 3 } `shouldEqual` { x: Just 3 }
      -- justify { x: (pure 3) } `shouldEqual` { x: Right 3 :: Either String Int }
  describe "justifill" do
    it "wraps values in Just and fills records" do
      justifill {} `shouldEqual` { x: Nothing :: Maybe Int }
      justifill { name: "Mark", id: [] :: _ Int } `shouldEqual` { name: Just "Mark", age: Nothing :: Maybe Int, id: [] :: Array Int }
      justifill { name: "Mark", name2: "Philipp", id: [] :: _ Int } `shouldEqual` { name: Just "Mark", name2: Just "Philipp", age: Nothing :: Maybe Int, id: [] :: Array Int }
    -- These should all not compile!
    -- it "doesn't work for empty Arrays of the wrong type" do
      -- justifill ([] :: Array String) `shouldEqual` ([] :: Array Int)
      -- justifill {x:([] :: Array String)} `shouldEqual` {x:Nothing :: (Maybe (Array Int))}
      -- justifill {x:(Nothing :: Maybe String)} `shouldEqual` {x:Nothing :: (Maybe Int)}
    it "works for records that are already complete" do
      justifill { a: 12, b: Just 4 } `shouldEqual` { a: 12, b: Just 4}
      -- [TODO] Can this be fixed?
      -- justifill { a: 12, b: Nothing } `shouldEqual` { a: 12, b: Nothing :: Maybe Int }
    it "works in let bindings" do
      let
        inLet = justifill { a: "Hi" }
      inLet `shouldEqual` { a: Just "Hi"}
