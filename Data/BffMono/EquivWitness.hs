{-# LANGUAGE FlexibleContexts, Rank2Types  #-}
{-| 
  A restricted version of @EquivMap@, which just records equivalence. 
 -} 
module Data.BffMono.EquivWitness 
     (
       EquivWitness
     , equals, equate, empty
     , emptyMap 
     )
    where 

import qualified Data.BffMono.EquivMap as EM 


newtype EquivWitness k = EquivWitness (forall a. EM.EquivMap k a)

emptyMap :: EquivWitness k -> EM.EquivMap k a 
emptyMap (EquivWitness w) = w 

empty :: EquivWitness k 
empty = EquivWitness EM.empty 

equals :: Ord k => k -> k -> EquivWitness k -> (Bool , EquivWitness k)
equals x y (EquivWitness t) =
    let (r,t') = EM.equals x y t
    in (r, EquivWitness t') 

equate :: Ord k => k -> k -> EquivWitness k -> EquivWitness k 
equate x y (EquivWitness t) =
    let t' = EM.equate x y t 
    in EquivWitness t'