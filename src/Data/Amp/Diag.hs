{-# LANGUAGE DeriveFunctor #-}
module Data.Amp.Diag where
import Prelude
--
--      FOLD                UNFOLD
--
--
--                              unfoldF                          foldF
data                                          Fix f =
--               a -------------------->                   -----------------> a
--               |                      ^                |                    ^
--        coalg  |                      |                |                    | alg
--               |                      |                | unFix              | 
                                 Fix -- |                |                    |
--               |                      |                |                    |
--               v                      |                v                    |
--             f a -------------------->                  -----------------> f a
--                 fmap (unfoldF coalg)                    fmap (foldF alg)
                                            (f (Fix f))
unFix :: Fix f ->                           (f (Fix f))
unFix (Fix x) = x 
data StreamF e a = StreamF e a deriving (Functor,Show)
fmapUnfoldF :: (Functor f) => (a -> f a) -> f a -> f (Fix f)
fmapUnfoldF coalg = fmap (unfoldF coalg)
unfoldF  :: Functor f => (a -> f a) -> a -> Fix f
unfoldF coalg = Fix . (fmapUnfoldF coalg) . coalg
fmapFoldF alg = fmap (foldF alg)
foldF :: Functor f => (f a -> a) -> Fix f -> a
foldF alg = alg . (fmapFoldF alg) . unFix

alg :: StreamF Int [Int] -> [Int] 
alg (StreamF e a) = e : a 

notdiv p n = n `mod` p /= 0
coalg :: [Int] -> StreamF Int [Int]
coalg (p : ns) =  StreamF p (filter (notdiv p) ns) 

main = do print $ ((foldF alg) . (unfoldF coalg)) [2..]

