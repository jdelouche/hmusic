module Main where
import Prelude
import Data.Amp.Music.Midi.Midi as Midi
import Data.Amp.Text.Sep        as Sep
import Data.Amp.Text.Table      as Table
import Codec.Midi
import Data.Typeable
import System.IO
midchannels::[[(Int,[Char])]] -> [[(Ticks, Message)]]
midchannels = fmap (foldr (\x a -> (Midi.midi x)++a) [])
parser :: ([(Int, Int, String)] -> [[(Int,String)]]) -> String -> [[(Int,[Char])]]
parser f x = f $ Sep.sep x
tomidi :: ([(Int, Int, String)] -> [[(Int,String)]]) -> String -> [[(Ticks, Message)]]
tomidi f x = midchannels $ parser f x
codecmulti n = Midi { fileType = MultiTrack, timeDiv  = TicksPerBeat 24, Codec.Midi.tracks   = n }
loop o s d       = do 
                  putStr d
                  writeFile "mymusic.txt" d
                  let f = if o then Table.tableh else Table.tablev
                  if s then exportFile "mymusic.mid" $ codecmulti $ tomidi f d
                       else exportFile "mymusic.mid" $ codecmulti $ tomidi f []
                  x <- getChar
                  case x of
                       '|'       -> loop False s d
                       '/'       -> loop True  s d
                       'q'       -> putStrLn ""
                       'z'       -> loop o False d
                       'y'       -> loop o True d
                       '\DEL'    -> case d of
                                      [] -> loop o True []
                                      _  -> do let c = init d
                                               loop o True c
                       '\n'   -> do loop o True (d++[x])
                       _      -> do let n = d++[x]
                                    loop o True n
main         = do loop False True ""
test1 = do let x="a1 a2 a3 end\n"++
                  "b1 b2 b3 end\n"
           print x
           let y = parser Table.tableh x     
           print y
           let z = midchannels y   
           print z
           exportFile "mymusic.mid" $ codecmulti z

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
           exportFile "test.mid" $ codecmulti $ tomidi Table.tablev x
