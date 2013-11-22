{-# LANGUAGE FlexibleContexts, Rank2Types  #-}

module Language.CheapB18n.EquivWitness 
     (
       EquivWitness
     , equals, equate, empty
     , emptyMap 
     )
    where 

import qualified Language.CheapB18n.EquivMap as EM 


newtype EquivWitness k = EquivWitness (forall a. EM.EquivMap k a)

emptyMap (EquivWitness w) = w 

empty = EquivWitness EM.empty 

equals x y (EquivWitness t) =
    let (r,t') = EM.equals x y t
    in (r, EquivWitness t') 

equate x y (EquivWitness t) =
    let t' = EM.equate x y t 
    in EquivWitness t'