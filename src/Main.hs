module Main where
import Prelude
import Data.Ampli.Ampli
main =  do print $ fmap (fmap (\x -> ampli (Left x))) [["c2","d2"],["c1","d1"]]
