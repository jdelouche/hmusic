module Main where
import Prelude
import Data.Ampli.Ampli      as Midi
import Data.Ampli.Text.Sep   as Sep
import Data.Ampli.Text.Table as Table
import Codec.Midi
import Data.Typeable
midi = fmap (foldr (\x a -> (_R_ $ Midi.ampli (Left x))++a) [])
unRight (Right a) = a
(_R_)             = unRight
parser x          = _R_ $ Table.ampli $ Left ([1..],_R_ $ Sep.ampli $ Left (False,1,1,x))
tomidi x          = midi $ parser x
codecmulti n      = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
loop d            = do 
                       print $ d
                       print $ parser d
                       print $ tomidi d
                       exportFile "mymusic.mid" $ codecmulti $ tomidi d
                       x <- getChar
                       case x of
                            'q'    -> putStrLn ""
                            '\DEL' -> case d of
                                           [] -> loop []
                                           _  -> loop $ init d
                            _      -> loop $ d++[x]
main         = loop ""
