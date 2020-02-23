module Main where
import Prelude
import Data.Amp.Music.Midi.Midi as Midi
import Data.Amp.Text.Sep        as Sep
import Data.Amp.Text.Table      as Table
import Codec.Midi
import Data.Typeable
midchannels::[[(Int,[Char])]] -> [[(Ticks, Message)]]
midchannels = fmap (foldr (\x a -> (Midi.midi x)++a) [])
parser :: String -> [[(Int,[Char])]]
parser x = Table.table $ Sep.sep $ x
tomidi :: String -> [[(Ticks, Message)]]
tomidi x = midchannels $ parser $ x
codecmulti n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
loop d       = do 
                  exportFile "mymusic.mid" $ codecmulti $ tomidi d
                  x <- getChar
                  case x of
                       'q'    -> putStrLn ""
                       '\DEL' -> case d of
                                      [] -> loop []
                                      _  -> loop $ init d
                       _      -> loop $ d++[x]
main         = loop ""
test = do let x= tomidi $ "c1 d1 e1\n"++
                          "c1 d1 e1\n"++
                          "c1 d1 e1\n"++
                          "d1 e1 f1\n"++
                          "e1 f1 g1\n"++
                          "d1 e1 f1\n"++
                          "end end end"
          exportFile "test.mid" $ codecmulti $ x
