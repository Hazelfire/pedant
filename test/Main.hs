{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Test.Hspec ( hspec, describe )
import Spec.Parser qualified as ParserSpec

main :: IO ()
main = hspec $
  describe "Parser spec" ParserSpec.spec

