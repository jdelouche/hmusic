module Bach where
import Prelude
import Codec.Midi
import Data.Music
import System.IO
import Data.List.Split
type Tokens    = [Token]
type Notes     = [Note]
type Line      = String
type Note      = String
type Token     = String
data Data      = Line | Note
ravel        ::                                                 Midi -> IO ()
codec        ::                                 Track Ticks  -> Midi
preisner     ::                    Tokens -> Track Ticks -> Notes -> IO ()
rachmaninoff ::            Tokens -> Track Ticks -> Notes -> Line -> IO ()
gluck        ::            Tokens -> Track Ticks -> Notes -> Line -> IO ()
benevolo     ::                    Tokens -> Track Ticks -> Notes -> IO ()
barber       ::                          Tokens -> Notes -> Notes -> IO ()
poulenc      ::           Tokens -> Track Ticks -> Notes -> Notes -> IO ()
sibellius    ::                    Tokens -> Track Ticks -> Notes -> IO () 
lully        ::            Tokens -> Track Ticks -> Notes -> Line -> IO () 
beethoven    ::            Tokens -> Track Ticks -> Notes -> Line -> IO () 
arvopart     ::            Tokens -> Track Ticks -> Notes -> Line -> IO () 
vivaldi      ::                    Tokens -> Track Ticks -> Notes -> IO ()
debussy      ::            Tokens -> Track Ticks -> Notes -> Line -> IO () 
mozart       ::            Tokens -> Track Ticks -> Notes -> Line -> IO ()
wagner       ::                    Tokens -> Track Ticks -> Notes -> IO () 
bach         ::                    Tokens -> Track Ticks -> Notes -> IO () 
codec n               = Midi { fileType = MultiTrack,timeDiv  = TicksPerBeat 24,tracks   = [n] } 
ravel m               = exportFile "mymusic.mid" m
charpentier           = ravel . codec
benevolo l xs ys      = do charpentier xs ; bach l xs ys
preisner l x m        = do let (z:zs) = m in print $ z++","++(foldr(\a x -> x++a++",") "" (reverse zs))
gluck l n m x         = if (x=="q") then do preisner l n m else do rachmaninoff l n m x
rachmaninoff l n m x  = benevolo l n m
arvopart l xs ys x    = do let (n,m) = ((xs++(notmi x)),(ys++[x])) in gluck l n m x
barber l cory ys      = do print ("Erasing:"++(last ys)) ; print cory
vivaldi l xs ys       = do let (cor,cory) = ((init . init) xs, init ys) in poulenc l cor cory ys
poulenc l cor cory ys = do barber l cory ys ; benevolo l cor cory
sibellius l xs ys     = let (z:zs)=l in mozart zs xs ys z
lully     l xs ys x   = if (length l == 0) then mozart [] xs ys x else sibellius l xs ys
beethoven l xs ys x   = do let l=splitOn "," x in lully l xs ys x
debussy   l xs ys x   = if (x/="x") then arvopart l xs ys x else vivaldi l xs ys
mozart    l xs ys x   = if (x/="") then debussy l xs ys x else bach l xs ys
wagner    l xs ys     = do x<-getLine; beethoven l xs ys x
bach      l xs ys     = if (length l == 0 ) then wagner l xs ys else let (z:zs) = l in mozart zs xs ys z
