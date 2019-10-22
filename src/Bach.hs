module Bach where
import Prelude
import Codec.Midi
import Data.Music
import System.IO
import Data.List.Split
codec        :: Track Ticks -> Midi
ravel        :: Midi -> IO ()
benevolo     :: [String] -> Track Ticks -> [String] -> IO ()
preisner     :: (Show a1, Show a2) => [String] -> a1 -> a2 -> IO ()
rachmaninoff :: [String] ->Track Ticks -> [String] -> String -> IO ()
arvopart     :: [String] -> [(Int, Message)] -> [String] -> String -> IO ()
gluck        :: [String] -> Track Ticks -> [String] -> String -> IO ()
barber       :: Show a => [String] ->a -> [[Char]] -> IO ()
vivaldi      :: [String] -> [(Int, Message)] -> [String] -> IO ()
poulenc      :: [String] -> Track Ticks -> [String] -> [[Char]] -> IO ()
debussy      :: [String] -> [(Int, Message)] -> [String] -> [Char] -> IO ()
mozart       :: [String] -> [(Int, Message)] -> [String] -> [Char] -> IO ()
bach         :: [String] -> [(Int, Message)] -> [String] -> IO ()
wagner       :: p -> [(Int, Message)] -> [String] -> IO ()
beethoven    :: p -> [(Int, Message)] -> [String] -> [Char] -> IO ()
lully        :: [String] -> [(Int, Message)] -> [String] -> [Char] -> IO ()
sibellius    :: [String] -> [(Int, Message)] -> [String] -> IO ()
codec n               = Midi { fileType = MultiTrack,timeDiv  = TicksPerBeat 24,tracks   = [n] } 
ravel m               = exportFile "mymusic.mid" m
charpentier           = ravel . codec
benevolo l xs ys      = do charpentier xs ;print xs;print l; bach l xs ys
preisner l x m        = do print x; print m
gluck l n m x         = if (x=="q") then do return () else do rachmaninoff l n m x
rachmaninoff l n m x  = do preisner l x m ; benevolo l n m
arvopart l xs ys x    = do let (n,m) = ((xs++(notmi x)),(ys++[x])) in gluck l n m x
barber l cory ys      = do print ("Erasing:"++(last ys)) ; print cory
vivaldi l xs ys       = do let (cor,cory) = ((init . init) xs, init ys) in poulenc l cor cory ys
poulenc l cor cory ys = do barber l cory ys ; benevolo l cor cory
debussy l xs ys x     = if (x/="x") then arvopart l xs ys x else vivaldi l xs ys
mozart l xs ys x      = if (x/="") then debussy l xs ys x else bach l xs ys
bach l xs ys          = if (length l == 0 ) then wagner l xs ys else let (z:zs) = l in mozart zs xs ys z
wagner l xs ys        = do x<-getLine; beethoven l xs ys x
beethoven l xs ys x   = do let l=splitOn "," x in lully l xs ys x
lully l xs ys x       = if (length l /= -1) then sibellius l xs ys else mozart [] xs ys x
sibellius l xs ys     = let (z:zs)=l in mozart zs xs ys z
