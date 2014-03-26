{-| 
  The module provides an automatic way to construct a bidirectional
  transformation (rougly speaking, a getter/setter pair) 
  from a uni-directional transformation (or, a getter function).

  The module provides a class 'PackM'. Once we write a transformation of type 

  @
  h :: (Traversable src, Traversable tgt) => forall a m.PackM c a m => src a -> m (tgt a)
  @ 

  then applying 'fwd' to obtain a forward transformation (so-called \"get\" or \"getter\") 
  
  @
  fwd h :: src c -> tgt c 
  @

  and applying `bwd` to obtain a backward transformation (so-called \"put\" or \"setter\").

  @
  bwd h :: (MonadError e m, Error e) => src c -> tgt c -> m (src c)
  @

  assuming that @c@ is some concrete type and @src@ and @tgt@ are some 
  concrete containers ('Data.Traversable' instances) with @Eq c@ and @Eq (tgt ())@.

  The correctness of the obtained bidirectional transformation 
  (GetPut and PutGet) is guaranted for free. That is, the following laws hold
  (assuming that we use @'Either' 'String'@ for the result of 'bwd').

  prop> bwd h s (fwd h s) = Right s 
  prop> bwd h s v = Right s'  implies fwd h s' = v  

  
-}

module Data.BffMono
    (
     Pack(..), PackM(..), liftO1, liftO2, 
     fwd, bwd
    ) where 

import Data.BffMono.Base

