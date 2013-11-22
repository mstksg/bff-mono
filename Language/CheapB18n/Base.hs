{-#
  LANGUAGE 
    FlexibleInstances, MultiParamTypeClasses, 
    FunctionalDependencies, Rank2Types,
    ImpredicativeTypes, FlexibleContexts 
  #-}


module Language.CheapB18n.Base where 

import Data.Traversable hiding (mapM)
import qualified Data.Traversable as T (mapM)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable 

import Data.Function (on) 

import Language.CheapB18n.CheckHistory 
-- import qualified Language.CheapB18n.EquivalenceClass as UF 


import Language.CheapB18n.EquivMap (EquivMap) 
import qualified Language.CheapB18n.EquivMap as EM 
import Language.CheapB18n.EquivWitness (EquivWitness)
import qualified Language.CheapB18n.EquivWitness as EW

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
       PackM conc abs m where
    liftO :: Eq r => ([conc] -> r) -> ([abs] -> m r)
    -- ^ lifting @conc@-level observations to @abs@ level, with 
    --   recording the examined values and the observed result. 

    eqSync :: Eq conc => abs -> abs -> m Bool 
    -- ^ lifting @conc@-level equivalence with synchronization 

    compareSync :: Ord conc => abs -> abs -> m Ordering 
    -- ^ lifting @conc@-level ordering.
    --   It synchronizes the elements if the comparison result is EQ 

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
data Location = InSource Int | InTrans
                deriving (Show, Eq, Ord)

-- | Datum with its pointer 
data Loc a = Loc { body :: a, location :: Location }
           deriving (Show, Eq, Ord)

-- | Update is a mapping from source locations to elements
-- type Update a = IntMap a
type Update a = EquivMap Int a 

----------------------------------------------------------------



-- | @update upd elem@ applies the update @upd@ to the source element @elem@.
-- update :: Update a -> Loc a -> (Loc a, Update a)
-- update upd (Loc a InTrans) = (Loc a Intrace, upd) 
-- update upd (Loc a (InSource i)) =
--     case EM.lookup i upd of 
--       (Nothing, upd') -> (Loc a (InSource i), upd')
--       (Just b , upd') -> (Loc b (InSource i), upd') 

-- update :: Update a -> Loc a -> Loc a
-- update upd (Loc a InTrans)        = Loc a InTrans
-- update upd (Loc a (InSource i)) =
--     case I.lookup i upd of 
--       Nothing -> Loc a (InSource i)
--       Just b  -> Loc b (InSource i)

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
      hasUpdated (Loc x _, y) = not (x == y) 
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
      isShapeEqual x y =  fmap (const ()) x == fmap (const ()) y 
          
          


-- matchViews :: (Eq a,Functor f,Foldable f, Eq (f ()), MonadError e m, Error e)
--               => f (Loc a) -> f a -> m (Update a)
-- matchViews xview view =
--     if isShapeEqual xview view then 
--         do { let pairs = zip (Foldable.toList xview) (Foldable.toList view)
--            ; pairs' <- mapM d pairs >>= (return . concat)
--            ; m <- 
--                foldM (\m (i,y) -> 
--                        case I.lookup i m of 
--                          Just z | z /= y -> 
--                              throwError errMsgInconsistent 
--                          Just _ -> 
--                              return m 
--                          Nothing -> 
--                              return $ I.insert i y m) I.empty pairs'
--            ; return $ shrink m }
--     else
--         throwError $ strMsg  "Shape Mismatch!"
--     where
--       initMap  = I.fromList $ 
--                    concatMap (\(Loc x y) -> 
--                                   case y of 
--                                     InTrans -> []
--                                     InSource i -> [(i,x)]) (Foldable.toList xview)
--       shrink m = I.differenceWith (\a b -> if a == b then Nothing else Just a) m initMap 

--       d (x,y) = case location x of 
--                   InSource i -> return [(i,y)]
--                   InTrans ->
--                       if body x == y then 
--                           return []
--                       else 
--                           throwError errMsgConstant 

--       isShapeEqual :: (Functor f, Eq (f ())) => f a -> f b -> Bool 
--       isShapeEqual x y =  fmap (const ()) x == fmap (const ()) y 



-- expandUpdate :: (MonadError e m, Error e, Eq a ) 
--                 => Update a -> UF.UnionFindTree Int -> m (Update a)
-- expandUpdate upd t =
--     evalStateT newUpd t 
--     where
--       cls x = do { t <- get 
--                  ; let (cs, t') = UF.equivalenceClass x t 
--                  ; return cs }
--       newUpd = initUpd >>= 
--                foldM (\upd (i,x) -> 
--                           case I.lookup i upd of 
--                             Just y -> 
--                                 if x == y then 
--                                     return upd 
--                                 else 
--                                     throwError errMsgInconsistent 
--                             Nothing -> 
--                                 return $ I.insert i x upd) I.empty 
--       initUpd =
--           mapM (\(i,v) -> 
--               do { cs <- cls i 
--                  ; return [ (j,v) | j <- cs ] }) (I.toList upd)
--           >>= (return . concat) 


------------------------------------------------------

-- | used internally 
instance Pack a (Identity a) where 
    new a = Identity a 

-- | used internally 
instance PackM a (Identity a) Identity where 
    liftO obs xs = return $ obs (map runIdentity xs)
    eqSync x y  = return $ runIdentity x == runIdentity y 
    compareSync x y = return $ runIdentity x `compare` runIdentity y 
                 
-- | used internally 
instance Pack a (Loc a) where 
    new a = Loc a InTrans 

-- type W a = WriterT (History (CheckResult (Loc a))) 
--                    (State (UF.UnionFindTree Int))


type B a = State ([CheckResult (Loc a)], EquivWitness Int)

-- unW :: W a b -> (b, History (CheckResult (Loc a)), UF.UnionFindTree Int)
-- unW m = 
--     let ((x,h),t) = runState (runWriterT m) UF.empty 
--     in  (x,h,t) 

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
          

-- -- | used internally 
-- instance PackM a (Loc a) (W a) where 
--     liftO obs xs = do { tell $ return $ CheckResult obs' xs (obs' xs)
--                       ; return $ obs' xs }
--         where obs' xs = obs (map body xs)

--     -- eqSync x y = if body x == body y then 
--     --                  do { t <- get 
--     --                     ; put $ UF.equate (location x) (location y) t     
--     --                     ; return True }
--     --              else
--     --                  do { _ <- liftO2 (==) x y 
--     --                     ; return False }
--     eqSync x y 
--         | body x == body y, InSource i <- location x, InSource j <- location y = 
--             do { t <- get 
--                ; put $ UF.equate i j t 
--                ; return True }
--         | otherwise = liftO2 (==) x y 

--     compareSync x y 
--         | EQ <- compare x y, InSource i <- location x, InSource j <- location y =
--              do { t <- get
--                 ; put $ UF.equate i j t
--                 ; return EQ }
--         | otherwise =
--             liftO2 compare x y 
            
--     -- compareSync x y = do { c <- liftO2 compare x y 
--     --                      ; case c of 
--     --                          EQ -> do { t <- get
--     --                                   ; put $ UF.equate (location x) (location y) t 
--     --                                   ; return EQ }
--     --                          GT -> return GT 
--     --                          LT -> return LT }
                                 
                          
                          

------------------------------------------------------

-- | Construction of a backward transformation (or, \"put\") from a
--   polymorphic function.
bwd :: (Eq (vf ()), Traversable vf, Traversable sf, Eq c,
        MonadError e n, Error e) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c -> n (sf c)
bwd pget src =
    \view ->
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


-- | Construction of a forward transformation (or, \"get\") from a
--   polymorphic function. 
fwd :: (Traversable vf, Traversable sf) =>
       (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
           sf c -> vf c
fwd pget =
    \src -> 
        let Identity r = pget $ fmap Identity src 
        in fmap runIdentity r


-- -- Maybe the following functions would be useful in practice. 

-- fwdI :: (Traversable vf, Traversable sf) =>
--        (forall a m. (PackM c a m) => sf a -> m (vf a)) ->
--            sf (Loc c) -> (vf (Loc c), History (CheckResult (Loc c)))
-- fwdI pget =
--     \src -> 
--         let (xview,hist) = runWriter $ pget src 
--         in (xview, hist)


-- bwdI :: (Traversable sf, Eq c, MonadError e n, Error e) =>
--            History (CheckResult (Loc c)) -> sf (Loc c)
--                -> Update c -> n (sf (Loc c))
-- bwdI hist xsrc upd =
--     if checkHistory (update upd) hist then 
--         return $ fmap (update upd) xsrc 
--     else
--         throwError $ strMsg "Violated Invariants"
