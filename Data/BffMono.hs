{-| 
  The module provides an automatic way to construct a bidirectional
  transformation (rougly speaking, a getter/setter pair) 
  from a uni-directional transformation (or, a getter function).

  The module provides a class `PackM`. Once we write a transformation of type 

  @
  h :: (Traversable src, Traversable tgt) => forall a m.PackM c a m => src a -> m (tgt a)
  @ 

  then applying `fwd` to obtain a forward transformation (so-called \"get\" or \"getter\") 
  
  @
  fwd h :: src c -> tgt c 
  @

  and applying `bwd` to obtain a backward transformation (so-called \"put\" or \"setter\").

  @
  bwd h :: (MonadError e m, Error e) => src c -> tgt c -> m (src c)
  @

  assuming that @c@ is some concrete type and @src@ and @tgt@ are some 
  concrete containers (`Traversable` instances) with @Eq c@ and @Eq (tgt ())@.

  The correctness of the obtained bidirectional transformation 
  (GetPut and PutGet) is guaranted for free. 
-}

module Data.BffMono
    (
     Pack(..), PackM(..), liftO1, liftO2, 
     fwd, bwd
    ) where 

import Data.BffMono.Base

