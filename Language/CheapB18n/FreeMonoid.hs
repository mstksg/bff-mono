{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
             

module Language.CheapB18n.FreeMonoid where 

import Data.Monoid 
import Control.Monad

import Data.Foldable (Foldable) 
import Data.Functor 


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
