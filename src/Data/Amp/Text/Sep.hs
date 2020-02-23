{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Text.Sep where
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
type Token    = (Int,Int,Char) 
type Transfer = Maybe Token 
type Receiver = (Bool,Int,Int,String)
type Sender   = [(Int,Int,String)]
elm::Int->Int->Sender->Bool
elm   c l s  = (or [x==c && y==l|(x,y,z)<-s]) 
firstelm::Int->Int->Char->Sender
firstelm  c l e    = [(c,l,[e])]
insertelm::Int->Int->Char->Sender->Sender
insertelm c l e s  = [(x,y,e:z) | (x,y,z)<-s,x==c && y==l]++[(x,y,z)|(x,y,z)<-s,not (x==c && y==l)]
addelm::Int->Int->Char->Sender->Sender
addelm    c l e s  = [(c,l,[e])]++s
table::Bool->Int->Int->Char->Sender->Sender
table _     c l e [] = firstelm c l e
table True  c l e s  = insertelm c l e s
table False c l e s  = addelm c l e s
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF Nothing         (Right s)) = (Right s)
send    (ChannelF (Just (c,l,e))  (Right s)) = (Right ((table (elm c l s) c l e s)))
send    (NilF)                               = (Right [])
receive (Left (p    ,_,_,    [])) = NilF
receive (Left (False,c,l, ' ':r)) = ChannelF Nothing          (Left (True,c+1,l  ,r))
receive (Left (True ,c,l, ' ':r)) = ChannelF Nothing          (Left (True,c  ,l  ,r))
receive (Left (p    ,c,l,'\n':r)) = ChannelF Nothing          (Left (False,1 ,l+1,r))
receive (Left (p    ,c,l,   e:r)) = ChannelF (Just (c,l,e))   (Left (False,c ,l  ,r))
unRight (Right x) = x
sep s = unRight $ amp $ Left (False,1,1,s)
test  = do print $ sep $ "11 21 31\n"++
                         "12 22 32\n"++
                         "13 23 33\n"
