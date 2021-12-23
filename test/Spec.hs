module Main (main) where

import Paths_pedant (getDataFileName)
import Pedant (EvaluationResult (..), evaluatePedantFile)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "GiveWell tests" $ do
    it "GiveDirectly" $ do
      fileName <- getDataFileName "examples/givewell/givedirectly.ped"
      resolveResult <- evaluatePedantFile fileName
      resolveResult `shouldSatisfy` isSuccess
    it "Against Malaria Foundation" $ do
      fileName <- getDataFileName "examples/givewell/amf.ped"
      resolveResult <- evaluatePedantFile fileName
      resolveResult `shouldSatisfy` isSuccess
  describe "Compilation tests" $ do
    it "allows imports" $ do
      fileName <- getDataFileName "examples/tests/simpleimport.ped"
      resolveResult <- evaluatePedantFile fileName
      resolveResult `shouldSatisfy` isSuccess
    it "allows functions" $ do
      fileName <- getDataFileName "examples/tests/functions.ped"
      resolveResult <- evaluatePedantFile fileName
      resolveResult `shouldSatisfy` isSuccess

isSuccess :: EvaluationResult -> Bool
isSuccess (EvaluationSuccess _) = True
isSuccess _ = False
