{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module Language.CheapB18n.CheckHistory where 

import Data.Monoid 
import Control.Monad

import Data.Foldable (Foldable) 
import qualified Data.Foldable as Foldable  


import Language.CheapB18n.FreeMonoid 

-- from mtl 
import Control.Monad.Writer 

-- | Datatype for history. We used a binary tree representation 
--   for O(1) append. 
type History a = FreeMonoid a 

-- | 'CheckResult' contains a check result of an observation function. 
data CheckResult a = forall b. Eq b => 
                     CheckResult ([a] -> b) [a] b 
                     

checkUpd :: (a -> a) -> CheckResult a -> Bool 
checkUpd u (CheckResult test as r) = 
    test (map u as) == r 


checkHistory :: (a -> a) -> History (CheckResult a) -> Bool 
checkHistory u = Foldable.all (checkUpd u) 
