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
type Receiver = (Int,[Char])
type Sender   = [(Ticks,Message)]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send   (ChannelF Nothing   (Right p))  = (Right p)
send   (ChannelF (Just i)  (Right p))  = (Right (p ++ i))
send   (NilF)                          = (Right [])
receive (Left (c,[]))           = NilF
receive (Left (c,('>':s)))      = ChannelF (Just [(0,Copyright s)])     (Left (c,[]))
receive (Left (c,"pause"))      = ChannelF (Just [(0,  NoteOn c 60 0),(24, NoteOff c 60 0)]) (Left (c,[]))
receive (Left (c,('#':s)))      = ChannelF (Just [(0,Text s)])          (Left (c,[]))
receive (Left (c,"end"))        = ChannelF (Just [(0,TrackEnd)])        (Left (c,[]))
receive (Left (c,"fin"))        = ChannelF (Just [(0,TrackEnd)])        (Left (c,[]))
receive (Left (c,(a:l:o:d:[]))) = ChannelF (Just (alod c (a:l:o:d:[]))) (Left (c,[]))
receive (Left (c,(a:l:o:[])))   = ChannelF (Just (alo  c (a:l:o:[])))   (Left (c,[]))
receive (Left (c,(l:o:[])))     = ChannelF (Just (lo   c (l:o:[])))     (Left (c,[]))
receive (Left (c,(l:[])))       = ChannelF (Just (lo   c (l:'1':[])))   (Left (c,[]))
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
                                     _   -> [(0, NoteOn c m      80),(24, NoteOff c m      0)] 
test = do print $ ampli $ Left (1,"a1")
          print $ ampli $ Left (1,"lb1-")
          print $ ampli $ Left (1,"b1_")
          print $ ampli $ Left (1,"c2_-")
          print $ ampli $ Left (1,"hc2__")
          print $ ampli $ Left (1,"d2")
