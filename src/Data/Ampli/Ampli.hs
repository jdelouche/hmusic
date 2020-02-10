{-# LANGUAGE DeriveFunctor #-}
module Data.Ampli.Ampli where
import Prelude
import Data.Ampli.Hylo
import Data.Music.Music
import Codec.Midi
data StreamF  e a = StreamF e a deriving (Functor,Show)
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier      = Either Receiver Sender
type Transfer     = Maybe [(Ticks,Message)]
type ConnectorF   = ChannelF Transfer
type InterfaceF   = ConnectorF Carrier
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
ampli   = (output . input)
type Receiver = [Char]
type Sender   = [(Ticks,Message)]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send   (ChannelF Nothing   (Right p))  = (Right p)
send   (ChannelF (Just i)  (Right p))  = (Right (p ++ i))
send   (NilF)                          = (Right [])
receive (Left [])           = NilF
receive (Left ('>':s))      = ChannelF (Just [(0,Copyright s)]) (Left [])
receive (Left "pause")      = ChannelF (Just [(0,  NoteOn 1 60 0),(24, NoteOff 1 60 0)]) (Left [])
receive (Left ('#':s))      = ChannelF (Just [(0,Text s)]) (Left [])
receive (Left "end")        = ChannelF (Just [(0,TrackEnd)]) (Left [])
receive (Left "fin")        = ChannelF (Just [(0,TrackEnd)]) (Left [])
receive (Left (a:l:o:d:[])) = ChannelF (Just (alod (a:l:o:d:[]))) (Left [])
receive (Left (p      :ns)) = ChannelF (Nothing) (Left ns)
alod (a:l:o:d:[]) = case d of
                         '-' -> fmap blanche (alo (a:l:o:[]))
                         '_' -> fmap (blanche . blanche) (alo (a:l:o:[]))
                         _   -> alo (a:l:o:[])
alo (a:l:o:[])   = case o of
                        '-' -> fmap blanche (lo (a:l:[]))
                        '_' -> fmap (blanche . blanche) (lo (a:l:[]))
                        _   -> case a of
                                      'l' -> fmap low (lo (l:o:[]))
                                      'h' -> fmap hight (lo (l:o:[]))
                                      _   -> lo (l:o:[])
lo (l:o:[]) = let m = case o of
                             '0' -> 48
                             '1' -> 60
                             '2' -> 72
                             '3' -> 84
                             '4' -> 96
                             _   -> 60
                             in case l of
                                     'c' -> [(0,  NoteOn 1 m      80),(24, NoteOff 1 m      0)] 
                                     'd' -> [(0,  NoteOn 1 (m+2)  80),(24, NoteOff 1 (m+2)  0)] 
                                     'e' -> [(0,  NoteOn 1 (m+4)  80),(24, NoteOff 1 (m+4)  0)] 
                                     'f' -> [(0,  NoteOn 1 (m+5)  80),(24, NoteOff 1 (m+5)  0)] 
                                     'g' -> [(0,  NoteOn 1 (m+7)  80),(24, NoteOff 1 (m+7)  0)] 
                                     'a' -> [(0,  NoteOn 1 (m+9)  80),(24, NoteOff 1 (m+9)  0)] 
                                     'b' -> [(0,  NoteOn 1 (m+11) 80),(24, NoteOff 1 (m+11) 0)] 
                                     _   -> [(0,  NoteOn 1 m      80),(24, NoteOff 1 m      0)] 

