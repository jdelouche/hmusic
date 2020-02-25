{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Music.Midi.Midi where
import Prelude
import Data.Amp.Hylo
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
amp     = (output . input)
type Receiver = (Int,[Char])
type Sender   = [(Ticks,Message)]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send   (ChannelF Nothing   (Right p))  = (Right p)
send   (ChannelF (Just i)  (Right p))  = (Right (p ++ i))
send   (NilF)                          = (Right [])
receive (Left (c,[]))           = NilF
receive (Left (c,('>':s)))      = ChannelF (Just [(0,Copyright s)])     (Left (c,[]))
receive (Left (c,"drums"))      = ChannelF (Just [(0,ProgramChange c 115)]) (Left (c,[]))
receive (Left (c,"pause"))      = ChannelF (Just [(0,  NoteOn c 60 0),(24, NoteOff c 60 0)]) (Left (c,[]))
receive (Left (c,"p"))          = ChannelF (Just [(0,  NoteOn c 60 0),(24, NoteOff c 60 0)]) (Left (c,[]))
receive (Left (c,('#':s)))      = ChannelF (Just [(0,Text s)])          (Left (c,[]))
receive (Left (c,"end"))        = ChannelF (Just [(0,TrackEnd)])        (Left (c,[]))
receive (Left (c,"fin"))        = ChannelF (Just [(0,TrackEnd)])        (Left (c,[]))
receive (Left (c,(a:l:o:d:[]))) = ChannelF (Just (alod c (a:l:o:d:[]))) (Left (c,[]))
receive (Left (c,(a:l:o:[])))   = ChannelF (Just (alo  c (a:l:o:[])))   (Left (c,[]))
receive (Left (c,(l:o:[])))     = ChannelF (Just (lo   c (l:o:[])))     (Left (c,[]))
receive (Left (c,(l:[])))       = ChannelF (Nothing)                    (Left (c,[]))
receive (Left (c,(p      :ns))) = ChannelF (Nothing)                    (Left (c,ns))
alod c (a:l:o:d:[]) = case d of
                         '-' -> fmap blanche (alo c (a:l:o:[]))
                         '_' -> fmap (blanche . blanche) (alo c (a:l:o:[]))
                         _   -> alo c (a:l:o:[])
alo c (x:y:z:[])   = case z of
                        '-' -> fmap blanche (lo c (x:y:[]))
                        '_' -> fmap (blanche . blanche) (lo c (x:y:[]))
                        _   -> case x of
                                      'l' -> fmap low   (lo c (y:z:[]))
                                      'h' -> fmap hight (lo c (y:z:[]))
                                      _   -> lo c (x:y:[])
lo c (l:o:[]) = let m = case o of
                             '0' -> 48
                             '1' -> 60
                             '2' -> 72
                             '3' -> 84
                             '4' -> 96
                             _   -> 60
                             in case l of
                                     'c' -> [(0, NoteOn c m      80),(24, NoteOff c m      0)] 
                                     'd' -> [(0, NoteOn c (m+2)  80),(24, NoteOff c (m+2)  0)] 
                                     'e' -> [(0, NoteOn c (m+4)  80),(24, NoteOff c (m+4)  0)] 
                                     'f' -> [(0, NoteOn c (m+5)  80),(24, NoteOff c (m+5)  0)] 
                                     'g' -> [(0, NoteOn c (m+7)  80),(24, NoteOff c (m+7)  0)] 
                                     'a' -> [(0, NoteOn c (m+9)  80),(24, NoteOff c (m+9)  0)] 
                                     'b' -> [(0, NoteOn c (m+11) 80),(24, NoteOff c (m+11) 0)] 
                                     _   -> [] 
lo c (l:[]) = []
low::(Ticks,Message) -> (Ticks,Message)
low      (d,NoteOn  x m v) = (d,NoteOn    x (m-1) v)
low      (d,NoteOff x m v) = (d,NoteOff   x (m-1) v)
low      m                 = m
hight::(Ticks,Message) -> (Ticks,Message)
hight    (d,NoteOn  x m v) = (d,NoteOn    x (m+1) v)
hight    (d,NoteOff x m v) = (d,NoteOff   x (m+1) v)
hight    m                 = m
blanche::(Ticks,Message) -> (Ticks,Message)
blanche  (d,NoteOn  x m v) = (d,NoteOn    x  m    v)
blanche  (d,NoteOff x m v) = (d+d,NoteOff x  m    v)
blanche  m                 = m
unRight (Right x) = x
midi x = unRight $ amp $ Left x
test = do print $ midi (1,"a1")
          print $ midi (2,"lb1-")
          print $ midi (3,"b1_")
          print $ midi (4,"c2_-")
