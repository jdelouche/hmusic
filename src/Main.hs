module Main where
import Prelude
import Data.Ampli.Ampli
main = do x <- getLine
          print $ ampli (Left x)
