{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Text.Table where
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
type Token    = (Int,String)
type Transfer = [Token]
type Sender   = [Transfer]
type Receiver = ([Int],[(Int,Int,String)])
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF line (Right s)) = (Right (line:s))
send    (NilF)                    = (Right [])
receive (Left (_   ,[])) = NilF
receive (Left ([]   ,_)) = NilF
receive (Left (p:ps,s )) = ChannelF (getline p s) (Left (ps,filterline p s))
getline    p s = [ (c,z)   |(c,l,z)<-s,c==p]
filterline p s = [ (c,l,z) |(c,l,z)<-s,c/=p]
test = do print $ amp 
                $ Left ([1..],[(1,1,"11"),(2,1,"21"),(3,1,"31"),
                               (1,2,"12"),(2,2,"22"),(3,2,"32"),
                               (1,3,"13"),(2,3,"23"),(3,3,"33")])
