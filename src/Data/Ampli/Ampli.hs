{-# LANGUAGE DeriveFunctor #-}
module Data.Ampli.Ampli where
import Prelude
import Data.Ampli.Hylo
data StreamF  e a = StreamF e a deriving (Functor,Show)
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier      = Either Receiver Sender
type Transfer     = Maybe Int
type ConnectorF   = ChannelF Transfer
type InterfaceF   = ConnectorF Carrier
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
ampli   = (output . input)
type Receiver = [Char]
type Sender   = [Int]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send   (ChannelF Nothing   (Right p))  = (Right p)
send   (ChannelF (Just i)  (Right p))  = (Right (i:p))
send   (NilF)                          = (Right [])
receive (Left [])           = NilF
receive (Left ('e':'1':ns)) = ChannelF (Just 48) (Left ns)
receive (Left ('e':'2':ns)) = ChannelF (Just 49) (Left ns)
receive (Left (p      :ns)) = ChannelF (Nothing) (Left ns)
