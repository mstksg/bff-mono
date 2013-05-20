{-# LANGUAGE
      Rank2Types, MultiParamTypeClasses, FunctionalDependencies,
      FlexibleInstances, UndecidableInstances
  #-}

-- | The module provides an embedded DSL for bidirectional
--   transformation that only concerns in-place updates. 
--   
--   The EDSL contains two operators |new| and |liftO|. Then, if we 
--   write a transformation of type 
--      |forall a m.PackM c a m => f a -> m (t a)|
--   for |Traversable| |f| and |t|, then applying |fwd| to obtain 
--   a forward transformation (so-called "get") and applying |bwd| to
--   obtain a backward transformation (so-called "put").


module Language.CheapB18n 
    (
     Pack, PackM, new, liftO, liftO1, liftO2, fwd, bwd      
    ) where 

import Language.CheapB18n.Helper 
import Language.CheapB18n.CheckHistory hiding (liftO)
import qualified Language.CheapB18n.CheckHistory as CH

import Data.Traversable 

-- from mtl
import Control.Monad.Identity 
import Control.Monad.Writer 
import Control.Monad.Error 


-- | |Pack conc abs| provides a way to abstract |conc| by |abs|. 
class Pack conc abs | abs -> conc where 
    new :: conc -> abs 

-- Functor m is required for convenience 
class 
    (Pack conc abs, Monad m, Functor m) => 
    PackM conc abs m | m -> abs where
      liftO :: Eq r => ([conc] -> r) -> ([abs] -> m r)

liftO1 :: (PackM conc abs m, Eq r) => (conc -> r) -> abs -> m r 
liftO1 f x = liftO (\[x] -> f x) [x]

liftO2 :: (PackM conc abs m, Eq r) 
          => (conc -> conc -> r) -> abs -> abs -> m r 
liftO2 f x y = liftO (\[x,y] -> f x y) [x,y]

------------------------------------------------------
instance Pack a (Identity a) where 
    new a = Identity a 

instance PackM a (Identity a) Identity where 
    liftO obs xs = return $ obs (map runIdentity xs)

instance Pack a (Loc a) where 
    new a = Loc a InTrans 

instance PackM a (Loc a) (Writer (History (CheckResult (Loc a)))) where 
    liftO obs = CH.liftO obs' 
        where obs' xs = obs (map body xs)
------------------------------------------------------


-- | Construction of a backward transformation (or, "put") from a
--   polymorphic function.
bwd :: (Eq (vf ()), Traversable vf, Traversable sf, Eq c,
        MonadError e n, Error e) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c -> n (sf c)
bwd pget =
    \src view ->
        do { let xsrc = assignIDs src
           ; let (xview, hist) = runWriter $ pget xsrc 
           ; upd <- matchViews xview view 
           ; if CH.checkHistory (update upd) hist then 
                 return $ fmap (body . update upd) xsrc 
             else
                 throwError $ strMsg "Violated Invariants"}


-- | Construction of a forward transformation (or, "get") from a
--   polymorphic function. 
fwd :: (Traversable vf, Traversable sf) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c
fwd pget =
    \src -> 
        let Identity r = pget $ fmap Identity src 
        in fmap runIdentity r



