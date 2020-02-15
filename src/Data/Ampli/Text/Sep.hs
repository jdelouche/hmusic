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
type Token    = (Int,Char) 
type Transfer = Maybe Token 
type Receiver = (Bool,Int,[Char])
type Sender   = [(Int,[Char])]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send    (ChannelF Nothing       (Right s))  = (Right s)
send    (ChannelF (Just (c,e))  (Right s))  = (Right (append c e s))
send    (NilF)                              = (Right [])
receive (Left (p,_,[]))     = NilF
receive (Left (p,c,' ':r))  = ChannelF Nothing      (Left $ if (p /= True) then (True,c+1,r) else (p,c,r))
receive (Left (p,c,'\n':r)) = ChannelF Nothing      (Left (False,1,r))
receive (Left (p,c,e:r))    = ChannelF (Just (c,e)) (Left (False,c,r))
append::Int->Char->Sender->Sender
append c e [] = (c,e:[]):[]
append c e s  = if (or $ fmap (\(i,x) -> i == c) s) then fmap (add c e) s else (c,e:[]):s
add::Int->Char->(Int,[Char])->(Int,[Char])
add c e (x,str) = if (c==x) then (x,e:str) else (x,str)
main = do print $ ampli $ Left (False,1,"a   b c d\ne f g h   \nj k l m\n")
