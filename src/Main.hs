module Main where
import Prelude
import Data.Ampli.Ampli as Midi
import Data.Ampli.Text.Sep as Sep
import Codec.Midi
import Data.Typeable
midi x a = [(c,l,s) | (c,l,s) <- a, c == x ]
midi2 y a = [midi x (c,l,s) | (c,l,s) <- a 
unRight (Right a) = a
(_R_)             = unRight
--parser x          =  _R_ $ midi $ _R_ $ Sep.ampli $ Left(False,1,1,x)
codecmulti n      = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
loop d            = do 
                       print $ d
                       print $ midi 1 $ _R_ $ Sep.ampli $ Left(False,1,1,d)
                       --print $ parser d
                       --exportFile "mymusic.mid" $ codecmulti $ parser d
                       x <- getChar
                       case x of
                            'q'    -> putStrLn ""
                            '\DEL' -> case d of
                                           [] -> loop []
                                           _  -> loop $ init d
                            _      -> loop $ d++[x]
main         = loop ""
