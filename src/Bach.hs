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
data Glob    = Glob { tokens :: Tokens, tracks :: Track Ticks, notes :: Notes } deriving (Eq,Show)
ravel        ::                               Midi -> IO ()
codec        ::               Track Ticks  -> Midi
preisner     ::                               Glob -> IO ()
gluck        ::                       Glob -> Line -> IO ()
benevolo     ::                               Glob -> IO ()
barber       ::           Tokens -> Notes -> Notes -> IO ()
poulenc      ::                      Glob -> Notes -> IO ()
sibellius    ::                               Glob -> IO () 
lully        ::                       Glob -> Line -> IO () 
beethoven    ::                       Glob -> Line -> IO () 
arvopart     ::                       Glob -> Line -> IO () 
vivaldi      ::                               Glob -> IO ()
debussy      ::                       Glob -> Line -> IO () 
mozart       ::                       Glob -> Line -> IO ()
wagner       ::                               Glob -> IO () 
bach         ::                               Glob -> IO () 
codec n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = [n] } 
ravel m                          = exportFile "mymusic.mid" m
charpentier                      = ravel . codec
benevolo  p@(Glob _ tr _)        = do charpentier tr ; bach p
preisner    (Glob _ x (z:zs))    = print $ z++","++(foldr(\a x -> x++a++",") "" (reverse zs))
gluck     p "q"                  = preisner p 
gluck     p  x                   = benevolo p 
arvopart    (Glob tko tr ns) x   = let (n,m) = ((tr++(notmi x)),(ns++[x])) in gluck (Glob tko n m) x
barber tko xtr ns                = do print ("Erasing:"++(last ns)) ; print xtr
vivaldi     (Glob l tr ns)       = let (xtr,xn) = ((init . init) tr, init ns) in poulenc (Glob l xtr xn) ns
poulenc   p@(Glob tko _ xns) ns  = do barber tko xns ns ; benevolo p
sibellius p@(Glob (t:ts) tr ns)  = mozart (Glob ts tr ns) t
lully     p@(Glob [] _ _) x      = mozart p x
lully     p x                    = sibellius p
beethoven p@(Glob _ tr ns)  x    = let tko'=splitOn "," x in lully (Glob tko' tr ns) x
debussy   p "x"                  = vivaldi p
debussy   p x                    = arvopart p x
mozart    p ""                   = bach p
mozart    p x                    = debussy p x
wagner    p                      = do x<-getLine; beethoven p x
bach      p@(Glob []  tr ns)     = wagner p
bach      p@(Glob (t:ts) tr ns)  = mozart (Glob ts tr ns) t
