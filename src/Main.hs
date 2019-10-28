module Main where
import Bach
import Codec.Midi
main :: IO ()
main = bach (Params [] [(0,Text "Start"),(0,Text "Start")] ["start"])
