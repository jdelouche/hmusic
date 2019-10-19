module Main where
import Prelude
import Codec.Midi
import Data.Music
import System.IO
codec :: Track Ticks -> Midi
codec n = Midi { fileType = MultiTrack,timeDiv  = TicksPerBeat 24,tracks   = [n] } 
ravel :: Midi -> IO ()
ravel m = exportFile "mymusic.mid" m
charpentier = ravel . codec
benevolo :: Track Ticks -> [String] -> IO ()
benevolo xs ys = do charpentier xs ;print xs; bach xs ys
preisner :: (Show a1, Show a2) => a1 -> a2 -> IO ()
preisner x m = do print x; print m
gluck :: Track Ticks -> [String] -> String -> IO ()
gluck n m x = if (x=="q") then do return () else do rachmaninoff n m x
rachmaninoff :: Track Ticks -> [String] -> String -> IO ()
rachmaninoff n m x = do preisner x m ; benevolo n m
arvopart::[(Int, Message)] -> [String] -> String -> IO ()
arvopart xs ys x = do let (n,m) = ((xs++(notmi x)),(ys++[x])) in gluck n m x
barber :: Show a => a -> [[Char]] -> IO ()
barber cory ys = do print ("Erasing:"++(last ys)) ; print cory
vivaldi::[(Int, Message)] -> [String] -> IO ()
vivaldi xs ys = do let (cor,cory) = ((init . init) xs, init ys) in poulenc cor cory ys
poulenc :: Track Ticks -> [String] -> [[Char]] -> IO ()
poulenc cor cory ys = do barber cory ys ; benevolo cor cory
debussy :: [(Int, Message)] -> [String] -> [Char] -> IO ()
debussy xs ys x = if (x/="x") then arvopart xs ys x else vivaldi xs ys
mozart :: [(Int, Message)] -> [String] -> [Char] -> IO ()
mozart xs ys x = if (x/="") then debussy xs ys x else bach xs ys
bach :: [(Int, Message)] -> [String] -> IO ()
bach xs ys = do x<-getLine ; mozart xs ys x
main :: IO ()
main = bach [(0,Text "Start"),(0,Text "Start")] ["start"]
