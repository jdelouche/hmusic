{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Text.Table where
import Prelude
import Data.Amp.Hylo
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier      = Receiver
type ConnectorF   = ChannelF Transfer
type InterfaceF   = ConnectorF Carrier
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
amp     = (output . input)
type Transfer = Int
type Sender   = Receiver
type Receiver = [Int]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF p (s)) = ((p:s))
send    (NilF)                 = ([])
receive ([])      = NilF
receive ((p:ps))  = ChannelF p (nomult p ps)
notdiv  p n = n `mod` p /= 0
nomult p = filter (notdiv p)
test = do print $ amp $ [2..]
