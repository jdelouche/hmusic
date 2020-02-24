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
type Receiver = (Bool,[Int],[(Int,Int,String)])
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF line (Right s)) = (Right (line:s))
send    (NilF)                    = (Right [])
receive (Left (_,_   ,[])) = NilF
receive (Left (_,[]   ,_)) = NilF
receive (Left (False,p:ps,s )) = ChannelF (getline p s) (Left (False,ps,filterline p s))
receive (Left (True,p:ps,s ))  = ChannelF (getcol  p s) (Left (True,ps,filtercol p s))
getline    p s = [ (c,z)   |(c,l,z)<-s,c==p]
filterline p s = [ (c,l,z) |(c,l,z)<-s,c/=p]
getcol     p s = [ (l,z)   |(c,l,z)<-s,l==p]
filtercol  p s = [ (c,l,z) |(c,l,z)<-s,l/=p]
unRight (Right x) = x
tableh x = unRight $ amp $ Left (True, [1..],x)
tablev x = unRight $ amp $ Left (False,[1..],x)
table = tablev
testv = do print $ tablev
                 $ [(1,1,"11"),(2,1,"21"),(3,1,"31"),
                    (1,2,"12"),(2,2,"22"),(3,2,"32"),
                    (1,3,"13"),(2,3,"23"),(3,3,"33")]
testh = do print $ tableh
                 $ [(1,1,"11"),(2,1,"21"),(3,1,"31"),
                    (1,2,"12"),(2,2,"22"),(3,2,"32"),
                    (1,3,"13"),(2,3,"23"),(3,3,"33")]
