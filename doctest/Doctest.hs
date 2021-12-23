module Main (main) where

import Build_doctests (flags, module_sources, pkgs)
import Test.DocTest (doctest)

main :: IO ()
main = do
  mapM_ putStrLn args
  doctest args
  where
    args = flags ++ pkgs ++ module_sources
