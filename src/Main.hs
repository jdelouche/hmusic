module Main where
import Prelude
import Data.Amp.Music.Midi.Midi as Midi
import Data.Amp.Text.Sep        as Sep
import Data.Amp.Text.Table      as Table
import Codec.Midi
import Data.Typeable
midi = fmap (foldr (\x a -> (_R_ $ Midi.amp (Left x))++a) [])
unRight (Right a) = a
(_R_)             = Main.unRight
parser x          = Table.table $ Sep.sep x
tomidi x          = midi $ parser x
codecmulti n      = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
loop d            = do 
                       exportFile "mymusic.mid" $ codecmulti $ tomidi d
                       x <- getChar
                       case x of
                            'q'    -> putStrLn ""
                            '\DEL' -> case d of
                                           [] -> loop []
                                           _  -> loop $ init d
                            _      -> loop $ d++[x]
main         = loop ""
