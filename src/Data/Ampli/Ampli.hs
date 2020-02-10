{-# LANGUAGE DeriveFunctor #-}
module Data.Ampli.Ampli where
import Prelude
import Data.Ampli.Hylo
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
receive (Left "do")         = ChannelF (Just [(0,  NoteOn 1 60 0),(24, NoteOff 1 60 0)]) (Left [])
receive (Left ('#':s))      = ChannelF (Just [(0,Text s)]) (Left [])
receive (Left (p      :ns)) = ChannelF (Nothing) (Left ns)
