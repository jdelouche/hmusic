{-# LANGUAGE DeriveFunctor #-}
module Data.Ampli.Ampli where
import Prelude
import Data.Typeable
data Fix f = Fix (f (Fix f))
instance Show (Fix f) where show (Fix x)= "."
unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg  = alg . fmap (cata alg) . unFix;
ana  :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
data StreamF  e a = StreamF e a deriving (Functor,Show)
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier    = Either Sender Receiver
type Transfer   = Maybe Int
type ConnectorF = ChannelF Transfer
type InterfaceF = ConnectorF Carrier
ampli = (output . input)
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
type Receiver = [Int]
type Sender   = [Char]
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send   (ChannelF Nothing   (Right p))  = (Right p)
send   (ChannelF (Just i)  (Right p))  = (Right (i:p))
send   (NilF)                         = (Right [])
receive (Left [])           = NilF
receive (Left ('e':'1':ns)) = ChannelF (Just 48) (Left ns)
receive (Left ('e':'2':ns)) = ChannelF (Just 49) (Left ns)
receive (Left (p : ns))     = ChannelF (Nothing) (Left ns)
main = do print $ ampli (Left "h1 e2")
