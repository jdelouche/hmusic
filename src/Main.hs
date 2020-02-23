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
loop s d       = do 
                  putStr d
                  if s then exportFile "mymusic.mid" $ codecmulti $ tomidi d
                       else exportFile "mymusic.mid" $ codecmulti $ tomidi []
                  x <- getChar
                  case x of
                       'q'       -> putStrLn ""
                       'z'       -> loop False d
                       'y'       -> loop True d
                       '\DEL'    -> case d of
                                      [] -> loop True []
                                      _  -> do let c = init d
                                               loop True c
                       '\n'   -> do loop True (d++[x])
                       _      -> do let n = d++[x]
                                    loop True n
main         = do loop True ""
test1 = do let x="a1 a2 a3\n"++
                 "b1 b2 b3\n"
           print x
           let y = parser x     
           print y
           let z = midchannels y   
           print z

test2 = do let x= "c1  pause    pause\n"++
                  "c1  pause    pause\n"++
                  "c1  pause    pause\n"++
                  "d1  f1       g1\n"++
                  "e1- g1-      a1-\n"++
                  "d1  pause    pause\n"++
                  "c1  pause    pause\n"++
                  "e1  pause    pause\n"++
                  "d1  pause    pause\n"++
                  "d1  pause    pause\n"++
                  "c1_ e1_      f1_\n"++
                  "end end      end"
           exportFile "test.mid" $ codecmulti $ tomidi x
