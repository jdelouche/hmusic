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
sibellius    ::                                                Ps -> IO () 
lully        ::                                        Ps -> Line -> IO () 
beethoven    ::                                        Ps -> Line -> IO () 
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
sibellius p@(Ps l  xs ys)    = let (z:zs)=l in mozart (Ps zs xs ys) z
lully     p@(Ps [] xs ys) x = mozart (Ps [] xs ys) x
lully     p@(Ps l xs ys) x  = sibellius p
beethoven p@(Ps l xs ys) x = do let l'=splitOn "," x in lully (Ps l' xs ys) x
debussy   p "x"            = vivaldi p
debussy   p x              = arvopart p x
mozart p@(Ps l xs ys) ""   = bach p
mozart p@(Ps l xs ys) x    = debussy p x
wagner p@(Ps tko tr ns)    = do x<-getLine; beethoven p x
bach p@(Ps []  tr ns)      = wagner p
bach p@(Ps tko tr ns)      = let (t:ts) = tko in mozart (Ps ts tr ns) t
