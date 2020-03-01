{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Math.Primes (primes) where
import Prelude
import Data.Amp.Hylo
data StreamF  e a = StreamF e a deriving (Functor,Show)
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier      = Either Receiver Sender
type ConnectorF   = ChannelF Transfer
type InterfaceF   = ConnectorF Carrier
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
amp     = (output . input)
type Transfer = Int
type Sender   = [Transfer]
type Receiver = [Int]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF p (Right s)) = (Right (p:s))
send    (NilF)                 = (Right [])
receive (Left [])      = NilF
receive (Left (p:ps))  = ChannelF p (Left $ nomult p ps)
notdiv  p n = n `mod` p /= 0
nomult p = filter (notdiv p)
unRight (Right x) = x
primes n = unRight $ amp $ Left [2..n]
test = do print $ primes 100
