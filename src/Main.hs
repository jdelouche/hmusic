module Main where
import Prelude
import Bach
import Codec.Midi
import Data.List.Split
import System.IO
-- bach (Glob 1 [] [(0,Text "Start"),(0,Text "Start")] ["start"])
--
check ts ('q':xs) = do
                       let (tr1,tr2,tr3) = (foldr (\t trs -> case t of
                                                                  (a:[])     -> a:trs
                                                                  (a:b:[])   -> a:trs
                                                                  (a:b:c:[]) -> a:trs
                                                                  _          -> "_":trs) [] ts,
                                            foldr (\t trs -> case t of 
                                                                  (a:b:[])   -> b:trs
                                                                  (a:b:c:[]) -> b:trs
                                                                  _          -> "pause":trs) [] ts,
                                            foldr (\t trs -> case t of 
                                                                  (a:b:[])   -> "pause":trs
                                                                  (a:b:c:[]) -> c:trs
                                                                  _          -> "pause":trs) [] ts)
                           in bach (Glob 1 tr1 [(0,Text "Start"),(0,Text "Start")] ["start"])
check y  _ = looping y
looping y  = do x <- getLine
                check (y ++ [splitOn " " x]) x
main :: IO ()
main = looping []
