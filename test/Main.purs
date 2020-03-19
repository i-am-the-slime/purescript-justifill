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
      justify [4] `shouldEqual` (Just [4])
      -- requires type annotation :(
      justify ([] :: _ Int) `shouldEqual` (Just ([]:: Array Int))
      justify { x: 4 } `shouldEqual` { x: 4 }
      justify { x: 4 } `shouldEqual` { x: Just 4 }
      justify { x: [1,2] } `shouldEqual` { x: Just ([1,2]) }
  describe "justifill" do
    it "wraps values in Just and fills records" do
      justifill {} `shouldEqual` { x: Nothing :: Maybe Int }
      justifill { name: "Mark", id: [] :: _ Int } `shouldEqual` { name: Just "Mark", age: Nothing :: Maybe Int, id: [] :: Array Int }
