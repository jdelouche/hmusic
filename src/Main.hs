module Main where
import Prelude
import Codec.Midi
import Data.List.Split
import System.IO
import Data.Music
import Data.Typeable
import Control.Monad
data GMain = Gmain { tkos::[[String]] }
tup f1 f2 f3 = foldr (\t trs -> case t of
                            (a:[])     -> f1 a trs
                            (a:b:[])   -> f2 a b trs
                            (a:b:c:[]) -> f3 a b c trs
                            _          -> "?":trs) []
one = tup (\a trs -> a:trs) (\a _ trs -> a:trs) (\a _ _ trs -> a:trs)
two = tup (\_ trs -> trs)   (\_ b trs -> b:trs) (\_ b _ trs -> b:trs)
thr = tup (\_ trs -> trs)   (\_ _ trs -> trs)   (\_ _ c trs -> c:trs)
codecmulti n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
looping (Gmain y) = do allx@(x:xs) <- getLine
                       case x of
                              'q' -> do let trs@(tr1,tr2,tr3) = (one y,two y,thr y)
                                        putStr "Channel 1> "
                                        print tr1
                                        putStr "Channel 2> "
                                        print tr2
                                        putStr "Channel 3> "
                                        print tr3
                                        let m1 = (foldr (\x a -> (notmi 1 x)++a ) [] tr1)
                                        let m2 = (foldr (\x a -> (notmi 1 x)++a ) [] tr2)
                                        let m3 = (foldr (\x a -> (notmi 1 x)++a ) [] tr3)
                                        exportFile "mymusic.mid" (codecmulti [m1,m2,m3])
                              _   -> readLine y allx
readLine y x = do let f = filter (/="") (splitOn " " x)
                  looping (Gmain (y++[f]))
main :: IO ()
main = looping (Gmain [])
