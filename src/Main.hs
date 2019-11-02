module Main where
import Prelude
import Codec.Midi
import Data.List.Split
import System.IO
data GMain = Gmain { tkos::[[String]] }
tup f1 f2 f3 = foldr (\t trs -> case t of
                            (a:[])     -> f1 a trs
                            (a:b:[])   -> f2 a b trs
                            (a:b:c:[]) -> f3 a b c trs
                            _          -> "?":trs) []
one = tup (\a trs -> a:trs)       (\a _ trs -> a:trs)       (\a _ _ trs -> a:trs)
two = tup (\_ trs -> "pause":trs) (\_ b trs -> b:trs)       (\_ b _ trs -> b:trs)
thr = tup (\_ trs -> "pause":trs) (\_ _ trs -> "pause":trs) (\_ _ c trs -> c:trs)
looping (Gmain y) = do (allx@(x:xs)) <- getLine
                       case x of
                              'q' -> do let trs@(tr1,tr2,tr3) = (one y,two y,thr y)
                                        print trs
                              _   -> looping (Gmain (y++[splitOn " " allx]))
main :: IO ()
main = looping (Gmain [])
