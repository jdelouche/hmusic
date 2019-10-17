module Main where
import Prelude
import Codec.Midi
import Data.Music
import System.IO
codec :: Track Ticks -> Midi
codec n = Midi { fileType = MultiTrack,timeDiv  = TicksPerBeat 24,tracks   = [n] } 
writem :: Midi -> IO ()
writem m = exportFile "mymusic.mid" m
push = writem . codec
midiout :: Track Ticks -> [String] -> IO ()
midiout xs ys = do push xs ; midadd xs ys
showAddition :: (Show a1, Show a2) => a1 -> a2 -> IO ()
showAddition x m = do print x; print m
maybeAddition :: Track Ticks -> [String] -> String -> IO ()
maybeAddition n m x = if (x=="q") then do return () else do add n m x
add :: Track Ticks -> [String] -> String -> IO ()
add n m x = do showAddition x m ; midiout n m
addition::[(Int, Message)] -> [String] -> String -> IO ()
addition xs ys x = do let (n,m) = ((xs++(notmi x)),(ys++[x])) in maybeAddition n m x
showCorrection :: Show a => a -> [[Char]] -> IO ()
showCorrection cory ys = do print ("Erasing:"++(last ys)) ; print cory
correction::[(Int, Message)] -> [String] -> IO ()
correction xs ys = do let (cor,cory) = ((init . init) xs, init ys) in change cor cory ys
change :: Track Ticks -> [String] -> [[Char]] -> IO ()
change cor cory ys = do showCorrection cory ys ; midiout cor cory
edition :: [(Int, Message)] -> [String] -> [Char] -> IO ()
edition xs ys x = if (x/="x") then addition xs ys x else correction xs ys
maybeMidadd :: [(Int, Message)] -> [String] -> [Char] -> IO ()
maybeMidadd xs ys x = if (x/="") then edition xs ys x else midadd xs ys
midadd :: [(Int, Message)] -> [String] -> IO ()
midadd xs ys = do x<-getLine ; maybeMidadd xs ys x
main :: IO ()
main = midadd [(0,Text "Pad"),(0,Text "Pad")] [""]
