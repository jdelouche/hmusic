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
ravel        ::                                              Midi -> IO ()
codec        ::                              Track Ticks  -> Midi
preisner     ::                                                Ps -> IO ()
gluck        ::                                        Ps -> Line -> IO ()
benevolo     ::                                                Ps -> IO ()
barber       ::                          Tokens -> Notes -> Notes -> IO ()
poulenc      ::                                       Ps -> Notes -> IO ()
sibellius    ::                                                Ps -> IO () 
lully        ::                                        Ps -> Line -> IO () 
beethoven    ::                                        Ps -> Line -> IO () 
arvopart     ::                                        Ps -> Line -> IO () 
vivaldi      ::                                                Ps -> IO ()
debussy      ::                                        Ps -> Line -> IO () 
mozart       ::                                        Ps -> Line -> IO ()
wagner       ::                                                Ps -> IO () 
bach         ::                                                Ps -> IO () 
codec n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = [n] } 
ravel m                        = exportFile "mymusic.mid" m
charpentier                    = ravel . codec
benevolo p@(Ps _ tr ns)        = do charpentier tr ; bach p
preisner (Ps _ x (z:zs))       = print $ z++","++(foldr(\a x -> x++a++",") "" (reverse zs))
gluck p "q"                    = preisner p 
gluck p  x                     = benevolo p 
arvopart (Ps tko tr ns) x      = let (n,m) = ((tr++(notmi x)),(ns++[x])) in gluck (Ps tko n m) x
barber tko xtr ns              = do print ("Erasing:"++(last ns)) ; print xtr
vivaldi (Ps l tr ns)           = let (xtr,xn) = ((init . init) tr, init ns) in poulenc (Ps l xtr xn) ns
poulenc p@(Ps tko _ xns) ns   = do barber tko xns ns ; benevolo p
sibellius p@(Ps (t:ts)  tr ns) = mozart (Ps ts tr ns) t
lully     p@(Ps [] tr ns) x    = mozart p x
lully     p x                  = sibellius p
beethoven p@(Ps tko tr ns) x   = let tko'=splitOn "," x in lully (Ps tko' tr ns) x
debussy   p "x"                = vivaldi p
debussy   p x                  = arvopart p x
mozart p@(Ps tko tr ns) ""     = bach p
mozart p@(Ps tko tr ns) x      = debussy p x
wagner p@(Ps tko tr ns)        = do x<-getLine; beethoven p x
bach p@(Ps []  tr ns)          = wagner p
bach p@(Ps (t:ts) tr ns)       = mozart (Ps ts tr ns) t
