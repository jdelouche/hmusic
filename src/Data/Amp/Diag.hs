{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Diag where
import Prelude
--
--      FOLD                UNFOLD
--
--
--                              unfoldF                          foldF
data                                           Fix f =
--                           a --------->                   --------------> a
--                           |           ^                |                 ^
--                    coalg  |           |                |                 | alg
--                           |           |                | unFix           | 
                                  Fix -- |                |                 |
--                           v           |                v                 |
--                         f a --------->                  -- -------------> f a
                                            (f (Fix f))
data StreamF e a =     StreamF e a 
                   deriving (Functor,Show)
--                             fmap foldF                 fmap unfoldF
--
fmapUnfoldF :: (Functor f) => (a -> f a) -> f a -> f (Fix f)
fmapUnfoldF coalg = fmap (unfoldF coalg)
unfoldF  :: Functor f => (a -> f a) -> a -> Fix f
unfoldF coalg = Fix . (fmapUnfoldF coalg) . coalg
fmapFoldF alg = fmap (foldF alg)
foldF :: Functor f => (f a -> a) -> Fix f -> a
foldF alg = alg . (fmapFoldF alg) . unFix

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x 

notdiv p n = n `mod` p /= 0
coalg :: [Int] -> StreamF Int [Int]
coalg (p : ns) =  StreamF p (filter (notdiv p) ns) 

alg :: StreamF Int [Int] -> [Int] 
alg (StreamF e a) = e : a 

main = do print $ ((foldF alg) . (unfoldF coalg)) [2..1000]

