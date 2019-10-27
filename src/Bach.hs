module Bach where
import Prelude
import Codec.Midi
import Data.Music
import System.IO
import Data.List.Split
type SplitCmds = [String]
type Notes     = [String]
type Line      = String
type Note      = String
data Data      = Line | Note
ravel        ::                                                 Midi -> IO ()
codec        ::                                 Track Ticks  -> Midi
benevolo     ::                    SplitCmds -> Track Ticks -> Notes -> IO ()
preisner     ::                    SplitCmds -> Track Ticks -> Notes -> IO ()
barber       ::                          SplitCmds -> Notes -> Notes -> IO ()
poulenc      ::           SplitCmds -> Track Ticks -> Notes -> Notes -> IO ()
bach         ::                    SplitCmds -> Track Ticks -> Notes -> IO () 
wagner       ::                    SplitCmds -> Track Ticks -> Notes -> IO () 
sibellius    ::                    SplitCmds -> Track Ticks -> Notes -> IO () 
vivaldi      ::                    SplitCmds -> Track Ticks -> Notes -> IO ()
rachmaninoff ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO ()
gluck        ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO ()
mozart       ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO ()
debussy      ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO () 
arvopart     ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO () 
beethoven    ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO () 
lully        ::            SplitCmds -> Track Ticks -> Notes -> Line -> IO () 
codec n               = Midi { fileType = MultiTrack,timeDiv  = TicksPerBeat 24,tracks   = [n] } 
ravel m               = exportFile "mymusic.mid" m
charpentier           = ravel . codec
benevolo l xs ys      = do charpentier xs ;print xs;print l; bach l xs ys
preisner l x m        = do print x; print m
gluck l n m x         = if (x=="q") then do return () else do rachmaninoff l n m x
rachmaninoff l n m x  = do preisner l n m ; benevolo l n m
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
