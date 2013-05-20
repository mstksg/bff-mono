module Language.CheapB18n.Helper where 


import Data.Traversable hiding (mapM)
import qualified Data.Traversable as T (mapM)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable 

import Data.Function (on) 

-- from containers 
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

-- from mlt 
import Control.Monad.Error 
import Control.Monad.State 


-- | Abstract Pointer
--   |InSource i| means i-th position in the original source. 
--   |InTrans| means outside of the original source.
data Index = InSource Int | InTrans
           deriving Show 

data Loc a = Loc { body :: a, index :: Index }
           deriving Show 

-- | Update is a mapping from source locations to elements
type Update a = IntMap a


instance Eq a => Eq (Loc a) where 
    (==) = (==) `on` body 

instance Ord a => Ord (Loc a) where
    compare = compare `on` body 


-- | |update upd elem| applies the update |upd| to the source element |elem|.
update :: Update a -> Loc a -> Loc a
update upd (Loc a InTrans)        = Loc a InTrans
update upd (Loc a (InSource i)) =
    case I.lookup i upd of 
      Nothing -> Loc a (InSource i)
      Just b  -> Loc b (InSource i)

-- | |assignIDs| assigns a distict |Index| for each source element. 
assignIDs :: Traversable f => f a -> f (Loc a) 
assignIDs t = 
    evalState (traverse f t) 0
        where
          f x = do { i <- get
                   ; put (i+1)
                   ; return $ Loc x (InSource i) }

isShapeEqual :: (Functor f, Eq (f ())) => f a -> f b -> Bool 
isShapeEqual x y =  fmap (const ()) x == fmap (const ()) y 

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



