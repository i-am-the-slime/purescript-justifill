module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Fill (full)
import Justifill.Definable (UndefinedOr, define, defined, undefined)
import Justifill.Fillable (fill)
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
      fill {} `shouldEqual` { x: undefined :: UndefinedOr Int }
      fill {} `shouldEqual` { x: undefined :: UndefinedOr Int, y: undefined :: UndefinedOr String }
      fill {} `shouldEqual` { x: undefined :: UndefinedOr { y :: Int } }
      fill {} `shouldEqual` { x: undefined :: UndefinedOr (Array Int) }
    it "fills partial records" do
      fill { x: undefined } `shouldEqual` { x: undefined :: UndefinedOr Int }
      fill { y: Just "hi", x: undefined } `shouldEqual` { x: undefined :: UndefinedOr Int, y: Just "hi" }
    it "leaves complete records" do
      fill { x: undefined, y: Just "Ho" } `shouldEqual` { x: undefined :: UndefinedOr Int, y: Just "Ho" }
  describe "define" do
    it "wraps values in UndefinedOr" do
      define 4 `shouldEqual` 4
      define 4 `shouldEqual` (defined 4)
      define "hi" `shouldEqual` (defined "hi")
      define [4] `shouldEqual` (defined [4])
      -- Back to using type annotations
      define ([] :: Array Int) `shouldEqual` ([] :: Array Int)
      define ([] :: _ Int) `shouldEqual` (defined ([] :: Array Int))
      define { x: 4 } `shouldEqual` { x: 4 }
      define { x: 4 } `shouldEqual` { x: defined 4 }
      define { x: [1,2] } `shouldEqual` { x: defined ([1,2]) }
      define { x: [1,2] } `shouldEqual` { x: defined ([1,2]) }
  describe "full" do
    it "wraps values in Just and fills records" do
      full {} `shouldEqual` {}
      full {} `shouldEqual` { heinz: (undefined :: _ Int), dembo: (undefined :: _ String), rambo: (undefined :: _ (Maybe Int) )}
      full { name: "Mark", id: [] :: _ Int } `shouldEqual` { name: defined "Mark", age: undefined :: UndefinedOr Int, id: [] :: Array Int }
      full { name: "Mark", name2: "Philipp", id: [] :: _ Int } `shouldEqual` { name: defined "Mark", name2: defined "Philipp", id: [] :: Array Int }
    -- These should all not compile!
    -- it "doesn't work for empty Arrays of the wrong type" do
      -- full ([] :: Array String) `shouldEqual` ([] :: Array Int)
      -- full {x:([] :: Array String)} `shouldEqual` {x: undefined :: (UndefinedOr (Array Int))}
      -- full {x:(undefined :: UndefinedOr String)} `shouldEqual` {x: undefined :: (UndefinedOr Int)}
    it "works for records that are already complete" do
      full { a: 12, b: defined 4 } `shouldEqual` { a: 12, b: defined 4}
      -- [TODO] Can this be fixed?
      -- full { a: 12, b: undefined } `shouldEqual` { a: 12, b: undefined :: UndefinedOr Int }
    it "works in let bindings" do
      let
        inLet = full { a: "Hi" }
      inLet `shouldEqual` { a: defined "Hi"}
