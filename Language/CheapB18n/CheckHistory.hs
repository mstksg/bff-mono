{-# LANGUAGE Rank2Types, DeriveFunctor, DeriveFoldable, 
             ExistentialQuantification, FlexibleContexts  #-}

module Language.CheapB18n.CheckHistory where 

import Data.Monoid 
import Control.Monad

import Data.Foldable (Foldable) 
import qualified Data.Foldable as Foldable  
import Data.Functor 

-- from mtl 
import Control.Monad.Writer 

-- | Datatype for history. We used a binary tree representation 
--   for O(1) append. 
type History a = FreeMonoid a 
data FreeMonoid a = MEmpty 
                  | MUnit a
                  | MAppend (FreeMonoid a) (FreeMonoid a)
                    deriving (Functor, Foldable)

instance Monad FreeMonoid where 
    return = MUnit 
    MEmpty >>= f  = MEmpty
    MUnit a >>= f = f a
    MAppend x y >>= f =
        MAppend (x >>= f) (y >>= f)

instance MonadPlus FreeMonoid where
    mzero = MEmpty 
    mplus = MAppend 

instance Monoid (FreeMonoid a) where 
    mempty  = MEmpty 
    mappend = MAppend 

-- | |CheckResult| contains a check result of an observation function. 
data CheckResult a = forall b. Eq b => 
                     CheckResult ([a] -> b) [a] b 
                     

checkUpd :: (a -> a) -> CheckResult a -> Bool 
checkUpd u (CheckResult test as r) = 
    test (map u as) == r 

liftO :: (Eq b, MonadWriter (History (CheckResult a)) m)
            => ([a] -> b) -> [a] -> m b
liftO f xs = do { tell $ return $ CheckResult f xs (f xs)
                ; return $ f xs }

-- liftO1 :: (Eq b, MonadWriter (History (CheckResult a)) m)
--           => (a -> b) -> a -> m b 
-- liftO1 f x = liftO g [x]
--     where g [x] = f x 

-- liftO2 :: (Eq b, MonadWriter (History (CheckResult a)) m)
--           => (a -> a -> b) -> a -> a -> m b 
-- liftO2 f x y = liftO g [x,y]
--     where g [x,y] = f x y 

checkHistory :: (a -> a) -> History (CheckResult a) -> Bool 
checkHistory u = Foldable.all (checkUpd u) 
