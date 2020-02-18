module Main where
import Prelude
import Control.Monad.State
import Data.Typeable
import Data.Music.Music
import Codec.Midi

type SplitValue = (String,[String])
type SplitState = (Bool, SplitValue)

split :: Char -> String -> State SplitState SplitValue
split sep ""     = do
    (state,(val,tab)) <- get
    case state of
         True -> put (False,("",tab))
         _    -> put (False,("",tab++[val]))
    return ("",[])

split sep (x:xs) = do
    (state, (val,tab)) <- get
    case x == sep of
         True | (state == True)  -> put (True, ("",tab))
         True | (state == False) -> put (True, ("",tab++[val]))
         _                      -> put (False, (val++[x],tab))
    split sep xs

startSplit = (True,("",[]))
doSplit :: Char -> String -> [String]
doSplit sep line = (snd $ snd $ execState (split sep line) (startSplit))

parse d      = fmap (\line -> (doSplit ' ' line)) $ doSplit '\n' d
col n        = fmap (\x -> x!!n)
track f t nbc = [fmap (\i -> f i t) [0..nbc-1]]
trackn d      = foldr  (\x a -> x ++ a) [] (track col d (mins d))
mins  d      = foldr min (maxBound::Int) (fmap length d)
notmis t     = foldr (\x a -> (notmi 1 x)++a ) [] t
codecmulti n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
loop d       = do x <- getChar
                  case x of
                       'q' -> do putStrLn ""
                                 let l = parse d
                                     m = trackn l
                                     score = fmap notmis m 
                                 print score
                                 exportFile "mymusic.mid" (codecmulti score)
                       _   -> loop $ d++[x]
main         = loop ""
