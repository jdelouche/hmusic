module Data.Music where
import Prelude
import Codec.Midi
notmi::Num a => Int -> String -> [(a, Message)]
notmi c "pause" = [(0,  NoteOn c 60 0),(24, NoteOff c 60 0)]
notmi c ('#':s) = [(0,Text s)]
notmi c "end"   = [(0,TrackEnd)]
notmi c "fin"   = [(0,TrackEnd)]
notmi c (a:l:o:d:[]) = case d of
                          '-' -> fmap blanche (notmi c (a:l:o:[]))
                          '_' -> fmap (blanche . blanche) (notmi c (a:l:o:[]))
                          _   -> notmi c (a:l:o:[])
notmi c (x:y:z:[]) = case z of
                          '-' -> fmap blanche (notmi c (x:y:[]))
                          '_' -> fmap (blanche . blanche) (notmi c (x:y:[]))
                          _   -> case x of
                                      'l' -> fmap low (notmi c (y:z:[]))
                                      'h' -> fmap hight (notmi c (y:z:[]))
                                      _   -> notmi c (y:z:[])
notmi c (l:o:[]) = let m = case o of
                             '0' -> 48
                             '1' -> 60
                             '2' -> 72
                             '3' -> 84
                             '4' -> 96
                             _   -> 60
                             in case l of
                                     'c' -> [(0,  NoteOn c m      80),(24, NoteOff c m      0)]
                                     'd' -> [(0,  NoteOn c (m+2)  80),(24, NoteOff c (m+2)  0)]
                                     'e' -> [(0,  NoteOn c (m+4)  80),(24, NoteOff c (m+4)  0)]
                                     'f' -> [(0,  NoteOn c (m+5)  80),(24, NoteOff c (m+5)  0)]
                                     'g' -> [(0,  NoteOn c (m+7)  80),(24, NoteOff c (m+7)  0)]
                                     'a' -> [(0,  NoteOn c (m+9)  80),(24, NoteOff c (m+9)  0)]
                                     'b' -> [(0,  NoteOn c (m+11) 80),(24, NoteOff c (m+11) 0)]
                                     _   -> [(0,  NoteOn c m      80),(24, NoteOff c m      0)]

notmi c s = [(0,Text s)]
low::(a, Message) -> (a, Message)
low      (d,NoteOn  x m v) = (d,NoteOn    x (m-1) v)
low      (d,NoteOff x m v) = (d,NoteOff   x (m-1) v)
low      m                 = m
hight::(a, Message) -> (a, Message)
hight    (d,NoteOn  x m v) = (d,NoteOn    x (m+1) v)
hight    (d,NoteOff x m v) = (d,NoteOff   x (m+1) v)
hight    m                 = m
blanche::Num a => (a, Message) -> (a, Message)
blanche  (d,NoteOn  x m v) = (d,NoteOn    x  m    v)
blanche  (d,NoteOff x m v) = (d+d,NoteOff x  m    v)
blanche  m                 = m
