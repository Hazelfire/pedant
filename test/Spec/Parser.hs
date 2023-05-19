{-# LANGUAGE OverloadedStrings #-}
module Spec.Parser (spec) where

import Test.Hspec
import Pedant.Parser (parseProgram)
import Data.Either (isRight)


spec :: Spec
spec = do
    describe "Parses programs with no line ending at end" $
        it "x = 2" $ do
            parseProgram "test" "x = 2" `shouldSatisfy` isRight