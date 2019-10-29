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
preisner     ::                                                Ps -> IO ()
gluck        ::                                        Ps -> Line -> IO ()
benevolo     ::                                                Ps -> IO ()
barber       ::                          Tokens -> Notes -> Notes -> IO ()
poulenc      ::                                       Ps -> Notes -> IO ()
sibellius    ::                                                Ps -> IO () 
lully        ::                                        Ps -> Line -> IO () 
beethoven    ::                                        Ps -> Line -> IO () 
arvopart     ::                                        Ps -> Line -> IO () 
vivaldi      ::                                               Ps  -> IO ()
debussy      ::                                        Ps -> Line -> IO () 
mozart       ::                                        Ps -> Line -> IO ()
wagner       ::                                                Ps -> IO () 
bach         ::                                                Ps -> IO () 
codec n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = [n] } 
ravel m                      = exportFile "mymusic.mid" m
charpentier                  = ravel . codec
benevolo p@(Ps l tr ys)      = do charpentier tr ; bach p
preisner (Ps _ x m)          = do let (z:zs) = m in print $ z++","++(foldr(\a x -> x++a++",") "" (reverse zs))
gluck p "q"                  = preisner p 
gluck p  x                   = benevolo p 
arvopart (Ps tko tr ys) x    = do let (n,m) = ((tr++(notmi x)),(ys++[x])) in gluck (Ps tko n m) x
barber tko cory ys           = do print ("Erasing:"++(last ys)) ; print cory
vivaldi (Ps l tr ys)         = do let (cor,cory) = ((init . init) tr, init ys) in poulenc (Ps l cor cory) ys
poulenc p@(Ps tko cor cory) ys = do barber tko cory ys ; benevolo p
sibellius p@(Ps tko  tr ys)  = let (t:ts)=tko in mozart (Ps ts tr ys) t
lully     p@(Ps [] tr ys) x  = mozart p x
lully     p x                = sibellius p
beethoven p@(Ps tko tr ys) x = do let l'=splitOn "," x in lully (Ps l' tr ys) x
debussy   p "x"              = vivaldi p
debussy   p x                = arvopart p x
mozart p@(Ps tko tr ys) ""   = bach p
mozart p@(Ps tko tr ys) x    = debussy p x
wagner p@(Ps tko tr ns)      = do x<-getLine; beethoven p x
bach p@(Ps []  tr ns)        = wagner p
bach p@(Ps tko tr ns)        = let (t:ts) = tko in mozart (Ps ts tr ns) t
