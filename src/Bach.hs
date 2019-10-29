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
data Ps    = Ps { tokens :: Tokens, tracks :: Track Ticks, notes :: Notes } deriving (Eq,Show)
ravel        ::                                                 Midi -> IO ()
codec        ::                                 Track Ticks  -> Midi
preisner     ::                    Tokens -> Track Ticks -> Notes -> IO ()
gluck        ::            Tokens -> Track Ticks -> Notes -> Line -> IO ()
benevolo     ::                                                Ps -> IO ()
barber       ::                          Tokens -> Notes -> Notes -> IO ()
poulenc      ::           Tokens -> Track Ticks -> Notes -> Notes -> IO ()
sibellius    ::                    Tokens -> Track Ticks -> Notes -> IO () 
lully        ::            Tokens -> Track Ticks -> Notes -> Line -> IO () 
beethoven    ::            Tokens -> Track Ticks -> Notes -> Line -> IO () 
arvopart     ::                                        Ps -> Line -> IO () 
vivaldi      ::                                               Ps  -> IO ()
debussy      ::                                        Ps -> Line -> IO () 
mozart       ::                                        Ps -> Line -> IO ()
wagner       ::                                                Ps -> IO () 
bach         ::                                                Ps -> IO () 
codec n              = Midi { fileType = MultiTrack,timeDiv  = TicksPerBeat 24,Codec.Midi.tracks   = [n] } 
ravel m              = exportFile "mymusic.mid" m
charpentier          = ravel . codec
benevolo p@(Ps l xs ys) = do charpentier xs ; bach p
preisner l x m       = do let (z:zs) = m in print $ z++","++(foldr(\a x -> x++a++",") "" (reverse zs))
gluck l n m x        = if (x=="q") then preisner l n m else benevolo (Ps l n m)
arvopart (Ps l xs ys) x   = do let (n,m) = ((xs++(notmi x)),(ys++[x])) in gluck l n m x
barber l cory ys     = do print ("Erasing:"++(last ys)) ; print cory
vivaldi (Ps l xs ys)      = do let (cor,cory) = ((init . init) xs, init ys) in poulenc l cor cory ys
poulenc l cor cory ys= do barber l cory ys ; benevolo (Ps l cor cory)
sibellius l xs ys    = let (z:zs)=l in mozart (Ps zs xs ys) z
lully     l xs ys x  = if (length l == 0) then mozart (Ps [] xs ys) x else sibellius l xs ys
beethoven l xs ys x  = do let l=splitOn "," x in lully l xs ys x
debussy   p x  = if (x/="x") then arvopart p x else vivaldi p
mozart p@(Ps l xs ys) x= if (x/="") then debussy p x else bach p
wagner (Ps tko tr ns)  = do x<-getLine; beethoven tko tr ns x
bach p@(Ps []  tr ns)  = wagner p
bach p@(Ps tko tr ns)  = let (t:ts) = tko in mozart (Ps ts tr ns) t
