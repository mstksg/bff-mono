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

emptyMap (EquivWitness w) = w 

empty = EquivWitness EM.empty 

equals x y (EquivWitness t) =
    let (r,t') = EM.equals x y t
    in (r, EquivWitness t') 

equate x y (EquivWitness t) =
    let t' = EM.equate x y t 
    in EquivWitness t'