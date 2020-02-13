{-# LANGUAGE DeriveFunctor #-}
module Data.Ampli.Text.Sep where
import Prelude
import Data.Ampli.Hylo
data StreamF  e a = StreamF e a deriving (Functor,Show)
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier      = Either Receiver Sender
type ConnectorF   = ChannelF Transfer
type InterfaceF   = ConnectorF Carrier
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
ampli   = (output . input)
type Transfer = Maybe (Int,Char) 
type Receiver = (Int,[Char])
type Sender   = [(Int,[Char])]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF Nothing       (Right s))  = (Right s)
send    (ChannelF (Just (c,e))  (Right s))  = (Right s)
send    (NilF)                              = (Right [])
receive (Left (_,[]))     = NilF
receive (Left (c,' ':r))  = ChannelF Nothing      (Left (c+1,r))
receive (Left (c,'\n':r)) = ChannelF Nothing      (Left (0,r))
receive (Left (c,e:r))    = ChannelF (Just (c,e)) (Left (c,r))
--addidx::Int->Char->Sender->Sender
--addidx c e = fmap (match c e)
--match c e (x,str) = if (c==x) then (x,str++[x]) else (x,str)
