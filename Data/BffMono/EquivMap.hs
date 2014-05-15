{-|

An implementation of maps based on Union-Find. 

-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.BffMono.EquivMap 
    (
     EquivMap
    , equals, equalsM
    , equate, equateM
    , lookup, lookupM
    , insert, insertM
    , empty
    ) 
    where 

import Prelude hiding (lookup)

import Data.IntMap (IntMap) 
import qualified Data.IntMap as IM
import Data.Map (Map) 
import qualified Data.Map as M 

import Data.Maybe (fromJust) 

import Control.Monad.State 

-- | A datastructure implements "map" based on Union-Find tree. 
data EquivMap k a =
    EquivMap {
      elemMap :: Map k Int,
       -- ^ mapping from keys to integers 
      tree :: IntMap Int,
       -- ^ body of union-find tree (negative value means -rank) 
      valueMap :: IntMap a
       -- ^ mapping to values (only correct for a root) 
    }

-- | Checks that two keys are in the same set or not. 
--   The resulting EquivMap is tuned for later queries. 
equals :: Ord k => k -> k -> EquivMap k a -> (Bool, EquivMap k a) 
equals x1 x2 t =
    let (r1,t1) = find x1 t 
        (r2,t2) = find x2 t1 
    in (r1 == r2, t2)

-- | Monadic-version of @equals@
equalsM :: (MonadState (EquivMap k a) m, Ord k) => k -> k -> m Bool 
equalsM x1 x2 =
    do { t <- get 
       ; let (r,t') = equals x1 x2 t
       ; put t' 
       ; return r }

-- | Empty map 
empty :: EquivMap k a 
empty = EquivMap M.empty IM.empty IM.empty 

-- | "find" of "Union-Find" 
find :: Ord k => k -> EquivMap k a -> (Int, EquivMap k a) 
find k equivMap =
    case M.lookup k (elemMap equivMap) of 
      Nothing -> 
          let i  = M.size (elemMap equivMap)
              equivMap' = 
                  equivMap { elemMap = M.insert k i (elemMap equivMap)
                           , tree = IM.insert i (-1) (tree equivMap) }
          in (i, equivMap')
      Just i -> 
          let (root, t') = findAux i (tree equivMap) 
          in (root, equivMap { tree = t' } )
    where
      findAux i t =
          let p = fromJust $ IM.lookup i t 
          in if p < 0 then 
                 (i,t) 
             else 
                 let (root,t') = findAux p t 
                 in (root, IM.insert i root t')

-- | Unify the set of the given two keys. This operation corresponds to 
--   "Union" of "Union-Find". 
--
--   FIXME: The function does not touch the values associated with the keys 
--          This does not affect the correctness of our bidirectionalization, 
--          which does not call @equate@ after @insert@ and @lookup@. 
equate :: Ord k => k -> k -> EquivMap k a -> EquivMap k a 
equate a1 a2 equivMap =
    let (root1, equivMap1) = find a1 equivMap
        (root2, equivMap2) = find a2 equivMap1
        rk1 = - (fromJust $ IM.lookup root1 $ tree equivMap2)
        rk2 = - (fromJust $ IM.lookup root2 $ tree equivMap2)
    in if root1 == root2 then 
           equivMap2 
       else
           if rk1 < rk2 then 
               insertUF root1 root2 rk1 rk2 (tree equivMap2) equivMap2
           else
               insertUF root2 root1 rk2 rk1 (tree equivMap2) equivMap2 
    where
      insertUF r1 r2 rk1 rk2 t em =
          let t'  = IM.insert r1 (- (rk1 + rk2)) t
              t'' = IM.insert r2 r1 t'
          in em { tree = t'' } 

-- | Monadic version of @equte@
equateM :: (MonadState (EquivMap k a) m, Ord k) => k -> k -> m ()
equateM a1 a2 = modify (equate a1 a2) 
       

-- | Associates a value to the set that contains the given key 
insert :: Ord k => k -> a -> EquivMap k a -> EquivMap k a 
insert k v equivMap =
    let (i, equivMap') = find k equivMap 
    in equivMap' { valueMap = IM.insert i v (valueMap equivMap') }

-- | A monadic version of @insert@ 
insertM :: (MonadState (EquivMap k a) m, Ord k) => k -> a -> m ()
insertM k v = modify (insert k v) 
    
-- | Loops-up values associated with the set of the given key 
lookup :: Ord k => k -> EquivMap k a -> (Maybe a, EquivMap k a) 
lookup k equivMap =
    let (i, equivMap') = find k equivMap 
    in (IM.lookup i (valueMap equivMap'), equivMap')

-- | A monadic version of @lookup@
lookupM :: (MonadState (EquivMap k a) m, Ord k) => k -> m (Maybe a)
lookupM k =
    do { t <- get 
       ; let (r,t') = lookup k t
       ; put t'
       ; return r }




                                   
                   
                   
              


                  