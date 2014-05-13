{-#
  LANGUAGE 
    FlexibleInstances, MultiParamTypeClasses, 
    FunctionalDependencies, RankNTypes,
    ImpredicativeTypes, FlexibleContexts, 
    PatternGuards 
  #-}


module Data.BffMono.Base where 

import Data.Traversable hiding (mapM)

import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable 

import Data.BffMono.CheckHistory 

import Data.BffMono.EquivMap (EquivMap) 
import qualified Data.BffMono.EquivMap as EM 
import Data.BffMono.EquivWitness (EquivWitness)
import qualified Data.BffMono.EquivWitness as EW

-- from mtl 
import Control.Monad.Error 
import Control.Monad.State 
import Control.Monad.Identity 

----------------------------------------------------------------
-- | @Pack conc abs@ provides a way to abstract @conc@ by @abs@. 
class Pack conc abs | abs -> conc where 
    new :: conc -> abs 

class (Pack conc abs, Monad m, Functor m) => 
       PackM conc abs m where
    liftO :: Eq r => ([conc] -> r) -> ([abs] -> m r)
    -- ^ Lifts @conc@-level observations to @abs@ level, with 
    --   recording the examined values and the observed result. 

    eqSync :: Eq conc => abs -> abs -> m Bool 
    -- ^ Lifts @conc@-level equivalence with synchronization 

    compareSync :: Ord conc => abs -> abs -> m Ordering 
    -- ^ Lifts @conc@-level ordering.
    --   It synchronizes the elements if the comparison result is EQ 

-- | A special version of 'liftO' for unary observations.
liftO1 :: (PackM conc abs m, Eq r) => (conc -> r) -> abs -> m r 
liftO1 f x = liftO (\[a] -> f a) [x]


-- | A special version of 'liftO' for binary observations.
liftO2 :: (PackM conc abs m, Eq r) 
          => (conc -> conc -> r) -> abs -> abs -> m r 
liftO2 f x y = liftO (\[a,b] -> f a b) [x,y]


-- | Abstract pointer.
--   @InSource i@ means i-th position in the original source. 
--   @InTrans@ means outside of the original source.
data Location = InSource Int | InTrans
                deriving (Show, Eq, Ord)

-- | Datum with its pointer 
data Loc a = Loc { body :: a, location :: Location }
           deriving (Show, Eq, Ord)

-- | Update is a mapping from source locations to elements
-- type Update a = IntMap a
type Update a = EquivMap Int a 

----------------------------------------------------------------



-- | @update elem@ applies the update in a state to the source element @elem@.

update :: MonadState (Update a) m => Loc a -> m (Loc a)
update (Loc a InTrans)      = return $ Loc a InTrans 
update (Loc a (InSource i)) = 
    do { r <- EM.lookupM i
       ; case r of 
           Nothing -> return $ Loc a (InSource i)
           Just b  -> return $ Loc b (InSource i)}



-- | 'assignIDs' assigns a distict 'Index' for each source element. 
assignIDs :: Traversable f => f a -> f (Loc a) 
assignIDs t = 
    evalState (traverse f t) 0
        where
          f x = do { i <- get
                   ; put (i+1)
                   ; return $ Loc x (InSource i) }

errMsgInconsistent :: Error e => e 
errMsgInconsistent = strMsg "Inconsistent Update!"

errMsgConstant :: Error e => e 
errMsgConstant = strMsg "Update on Constant!"


{- This version does not check the all the duplicates are updated as in 
   the same way -} 
matchViews :: (Eq a,Functor f,Foldable f, Eq (f ()), MonadError e m, Error e)
              => f (Loc a) -> f a -> EquivWitness Int -> m (Update a) 
matchViews xview view equiv =
    if isShapeEqual xview view then 
        makeUpd (EW.emptyMap equiv) $ filter hasUpdated
                    $ zip (Foldable.toList xview) (Foldable.toList view)
    else
        throwError $ strMsg "Shape Mismatch!"
    where
      hasUpdated (Loc x _, y) = x /= y 
      makeUpd upd [] = return upd 
      makeUpd upd ((Loc _ InTrans,y):ps) = throwError errMsgConstant 
      makeUpd upd ((Loc _ (InSource i), y):ps) =
          case EM.lookup i upd of 
            (Just z, upd') -> 
                if z == y then 
                    makeUpd upd' ps 
                else
                    throwError errMsgInconsistent 
            (Nothing, upd') -> 
                makeUpd (EM.insert i y upd) ps 
      isShapeEqual :: (Functor f, Eq (f ())) => f a -> f b -> Bool 
      isShapeEqual x y =  void x == void y 
          
          


------------------------------------------------------

-- | used internally 
instance Pack a (Identity a) where 
    new = Identity 

-- | used internally 
instance PackM a (Identity a) Identity where 
    liftO obs xs = return $ obs (map runIdentity xs)
    eqSync x y  = return $ runIdentity x == runIdentity y 
    compareSync x y = return $ runIdentity x `compare` runIdentity y 
                 
-- | used internally 
instance Pack a (Loc a) where 
    new a = Loc a InTrans 

-- We record checking histories by a State-monad for efficiency, 
-- unlike what written in the paper. 
type B a = State ([CheckResult (Loc a)], EquivWitness Int)


unB :: B a b -> (b, [CheckResult (Loc a)], EquivWitness Int)
unB m =
    let (x,(h,t)) = runState m ([], EW.empty)
    in (x,h,t)

-- | used internally 
instance PackM a (Loc a) (B a) where 
    liftO obs xs = do { modify (\(h,t) -> (CheckResult obs' xs (obs' xs):h,t))
                      ; return $ obs' xs }
        where
          obs' xs = obs (map body xs)

    eqSync x y 
        | body x == body y, InSource i <- location x, InSource j <- location y = 
              do { modify (\(h,t) -> (h, EW.equate i j t))
                 ; return True }
        | otherwise = liftO2 (==) x y 

    compareSync x y 
        |  EQ <- compare x y, InSource i <- location x, InSource j <- location y =
              do { modify (\(h,t) -> (h, EW.equate i j t))
                 ; return EQ }
        | otherwise = liftO2 compare x y 
          
                          

------------------------------------------------------

-- | Constructs a backward transformation (or, \"put\" or
--   \"setter\") from a given function.
bwd :: (Eq (vf ()), Traversable vf, Traversable sf, Eq c,
        MonadError e n, Error e) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c -> n (sf c)
bwd pget src view =
        do { upd <- matchViews xview view equiv 
           ; let (b,upd') = runState (checkHistory update hist) upd 
           ; if b then 
                 let u x = evalState (update x) upd' 
                 in return $ fmap (body . u) xsrc 
             else
                 throwError $ strMsg "Violated Invariants"}
    where
      xsrc = assignIDs src 
      (xview, hist, equiv) = unB' (pget xsrc) 
      -- for type inference 
      unB' = unB :: B c (sf (Loc c))
                    -> (sf (Loc c), 
                        [CheckResult (Loc c)], 
                        EquivWitness Int) 


-- | Constructs a forward transformation (or, \"get\" or \"getter\") from a
--   given function. 
fwd :: (Traversable vf, Traversable sf) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c
fwd pget src =
    let Identity r = pget $ fmap Identity src 
    in fmap runIdentity r

