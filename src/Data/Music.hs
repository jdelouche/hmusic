module Data.Music where
import Prelude
import Codec.Midi
notmi "pause" = [(0,  NoteOn 0 60 0),(24, NoteOff 0 60 0)]
notmi ('#':s) = [(0,Text s)]
notmi "end"   = [(0,TrackEnd)]
notmi "fin"   = [(0,TrackEnd)]
notmi (a:l:o:[]) = case a of
                        'l' -> fmap low (notmi (l:o:[]))
                        'h' -> fmap hight (notmi (l:o:[]))
                        _   -> notmi (l:o:[])
notmi (l:o:[]) = let m = case o of
                             '0' -> 48
                             '1' -> 60
                             '2' -> 72
                             '3' -> 84
                             '4' -> 96
                             _   -> 60
                             in case l of
                                     'c' -> [(0,  NoteOn 0 m      80),(48, NoteOff 0 m      0)]
                                     'd' -> [(0,  NoteOn 0 (m+2)  80),(48, NoteOff 0 (m+2)  0)]
                                     'e' -> [(0,  NoteOn 0 (m+4)  80),(48, NoteOff 0 (m+4)  0)]
                                     'f' -> [(0,  NoteOn 0 (m+5)  80),(48, NoteOff 0 (m+5)  0)]
                                     'g' -> [(0,  NoteOn 0 (m+7)  80),(48, NoteOff 0 (m+7)  0)]
                                     'a' -> [(0,  NoteOn 0 (m+9)  80),(48, NoteOff 0 (m+9)  0)]
                                     'b' -> [(0,  NoteOn 0 (m+11) 80),(48, NoteOff 0 (m+11) 0)]
                                     _   -> [(0,  NoteOn 0 m      80),(48, NoteOff 0 m      0)]

notmi s = [(0,Text s)]
low   (d,NoteOn x m v)  = (d,NoteOn x (m-1) v)
low   (d,NoteOff x m v) = (d,NoteOff x (m-1) v)
hight  (d,NoteOn x m v)  = (d,NoteOn x (m+1) v)
hight  (d,NoteOff x m v) = (d,NoteOff x (m+1) v)
