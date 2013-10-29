-- | The module provides an embedded DSL for bidirectional
--   transformation that only concerns in-place updates. 
--   
--   The EDSL provides a class 'PackM' that has the two methods 'new' and 'liftO'. 
--   Then, once we write a transformation of type 
--   @Traversable f t => forall a m.PackM c a m => f a -> m (t a)@, 
--   then applying 'fwd' to obtain 
--   a forward transformation (so-called \"get\") and applying 'bwd' to
--   obtain a backward transformation (so-called \"put\").
--
--   The correctness of the obtained bidirectional transformation 
--   (GetPut and PutGet) is guaranted for free. 


module Language.CheapB18n 
    (
     Pack, PackM, new, liftO, eqSync, liftO1, liftO2, 
     fwd, bwd
    ) where 

import Language.CheapB18n.Base

