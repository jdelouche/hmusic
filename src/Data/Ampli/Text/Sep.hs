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
send    (ChannelF (Just (c,e))  (Right s))  = (Right (append c e s))
send    (NilF)                              = (Right [])
receive (Left (_,[]))     = NilF
receive (Left (c,' ':r))  = ChannelF Nothing      (Left (c+1,r))
receive (Left (c,'\n':r)) = ChannelF Nothing      (Left (1,r))
receive (Left (c,e:r))    = ChannelF (Just (c,e)) (Left (c,r))
append::Int->Char->Sender->Sender
append c e [] = (c,e:[]):[]
append c e s  = if (or $ fmap (\(i,x) -> i == c) s) then fmap (add c e) s else (c,e:[]):s
add::Int->Char->(Int,[Char])->(Int,[Char])
add c e (x,str) = if (c==x) then (x,e:str) else (x,str)
main = do print $ ampli $ Left (1,"a b c d\ne f g h \nj k l m\n")
