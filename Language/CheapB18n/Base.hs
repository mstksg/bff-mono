{-#
  LANGUAGE 
    FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, 
    FunctionalDependencies, Rank2Types
  #-}


module Language.CheapB18n.Base where 

import Data.Traversable hiding (mapM)
import qualified Data.Traversable as T (mapM)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable 

import Data.Function (on) 

import Language.CheapB18n.CheckHistory 



-- from containers 
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

-- from mtl 
import Control.Monad.Error 
import Control.Monad.State 
import Control.Monad.Identity 
import Control.Monad.Writer 


----------------------------------------------------------------
-- | @Pack conc abs@ provides a way to abstract @conc@ by @abs@. 
class Pack conc abs | abs -> conc where 
    new :: conc -> abs 

class (Pack conc abs, Monad m, Functor m) => 
       PackM conc abs m | m -> abs where
    liftO :: Eq r => ([conc] -> r) -> ([abs] -> m r)
    -- ^ lifting @conc@-level observations to @abs@ level, with 
    --   recording the examined values and the observed result. 

-- | A special version of 'liftO' for unary observations.
liftO1 :: (PackM conc abs m, Eq r) => (conc -> r) -> abs -> m r 
liftO1 f x = liftO (\[x] -> f x) [x]


-- | A special version of 'liftO' for binary observations.
liftO2 :: (PackM conc abs m, Eq r) 
          => (conc -> conc -> r) -> abs -> abs -> m r 
liftO2 f x y = liftO (\[x,y] -> f x y) [x,y]



-- | Abstract pointer.
--   @InSource i@ means i-th position in the original source. 
--   @InTrans@ means outside of the original source.
data Index = InSource Int | InTrans
           deriving (Show, Eq, Ord)

-- | Datum with its pointer 
data Loc a = Loc { body :: a, index :: Index }
           deriving (Show, Eq, Ord)

-- | Update is a mapping from source locations to elements
type Update a = IntMap a

----------------------------------------------------------------



-- | @update upd elem@ applies the update @upd@ to the source element @elem@.
update :: Update a -> Loc a -> Loc a
update upd (Loc a InTrans)        = Loc a InTrans
update upd (Loc a (InSource i)) =
    case I.lookup i upd of 
      Nothing -> Loc a (InSource i)
      Just b  -> Loc b (InSource i)


-- | 'assignIDs' assigns a distict 'Index' for each source element. 
assignIDs :: Traversable f => f a -> f (Loc a) 
assignIDs t = 
    evalState (traverse f t) 0
        where
          f x = do { i <- get
                   ; put (i+1)
                   ; return $ Loc x (InSource i) }



matchViews :: (Eq a,Functor f,Foldable f, Eq (f ()), MonadError e m, Error e)
              => f (Loc a) -> f a -> m (Update a)
matchViews xview view =
    if isShapeEqual xview view then 
        do { let pairs = zip (Foldable.toList xview) (Foldable.toList view)
           ; pairs' <- mapM d pairs >>= (return . concat)
           ; m <- 
               foldM (\m (i,y) -> 
                       case I.lookup i m of 
                         Just z | z /= y -> 
                             throwError $ strMsg "Inconsistent Update!"
                         Just _ -> 
                             return m 
                         Nothing -> 
                             return $ I.insert i y m) I.empty pairs'
           ; return $ shrink m }
    else
        throwError $ strMsg  "Shape Mismatch!"
    where
      initMap  = I.fromList $ 
                   concatMap (\(Loc x y) -> 
                                  case y of 
                                    InTrans -> []
                                    InSource i -> [(i,x)]) (Foldable.toList xview)
      shrink m = I.differenceWith (\a b -> if a == b then Nothing else Just a) m initMap 

      d (x,y) = case index x of 
                  InSource i -> return [(i,y)]
                  InTrans ->
                      if body x == y then 
                          return []
                      else 
                          throwError $ strMsg "Update of Constant!"

      isShapeEqual :: (Functor f, Eq (f ())) => f a -> f b -> Bool 
      isShapeEqual x y =  fmap (const ()) x == fmap (const ()) y 

------------------------------------------------------

-- | used internally 
instance Pack a (Identity a) where 
    new a = Identity a 

-- | used internally 
instance PackM a (Identity a) Identity where 
    liftO obs xs = return $ obs (map runIdentity xs)

-- | used internally 
instance Pack a (Loc a) where 
    new a = Loc a InTrans 

-- | used internally 
instance PackM a (Loc a) (Writer (History (CheckResult (Loc a)))) where 
    liftO obs xs = do { tell $ return $ CheckResult obs' xs (obs' xs)
                      ; return $ obs' xs }
        where obs' xs = obs (map body xs)

------------------------------------------------------


-- | Construction of a backward transformation (or, \"put\") from a
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
           ; if checkHistory (update upd) hist then 
                 return $ fmap (body . update upd) xsrc 
             else
                 throwError $ strMsg "Violated Invariants"}


-- | Construction of a forward transformation (or, \"get\") from a
--   polymorphic function. 
fwd :: (Traversable vf, Traversable sf) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c
fwd pget =
    \src -> 
        let Identity r = pget $ fmap Identity src 
        in fmap runIdentity r


-- Maybe the following functions would be useful in practice. 

fwdI :: (Traversable vf, Traversable sf) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf (Loc c) -> (vf (Loc c), History (CheckResult (Loc c)))
fwdI pget =
    \src -> 
        let (xview,hist) = runWriter $ pget src 
        in (xview, hist)


bwdI :: (Traversable sf, Eq c, MonadError e n, Error e) =>
           History (CheckResult (Loc c)) -> sf (Loc c)
               -> Update c -> n (sf (Loc c))
bwdI hist xsrc upd =
    if checkHistory (update upd) hist then 
        return $ fmap (update upd) xsrc 
    else
        throwError $ strMsg "Violated Invariants"
