module Main (main) where
import Test.QuickCheck
import Data.Music
testGamme::IO ()
testGamme = do
  putStrLn "Testing Gamme"
  quickCheck prop_1
  where 
    prop_1::Integer->Bool
    prop_1 x = True
main :: IO ()
main = do
  testGamme
