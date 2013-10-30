{-|
 
A simple implementation for Union-Find tree. Although there already
have been at least three implementations in HackageDB (union-find,
equivalence, persistent-equivalence), We implemented another one
because of the following reasons.

 (1) Monadic implementation would complicate ourselves.
     (we use it with other monad).

 (2) We would like to gather the elements in an equivalence class.
     Actually, the operation is not necessary in our implementation, but 
     is convenient to reuse the existing codes. 

-} 
module Language.CheapB18n.EquivalenceClass 
    (
      UnionFindTree
    , empty, find, equate, equivalenceClass, equals 
    )
    where 

import Data.IntMap (IntMap) 
import qualified Data.IntMap as IM
import Data.Map (Map) 
import qualified Data.Map as M 

import Data.Monoid 
import Data.Maybe (fromJust) 

import qualified Data.Foldable 
import Data.List (groupBy, sortBy)
import Data.Function 

import Language.CheapB18n.FreeMonoid

data UnionFindTree a =
    UnionFindTree 
       (Map a Int) 
       (IntMap Int)
       -- ^ body of union-find tree (negative value means -rank) 
       (IntMap (FreeMonoid a)) 
       -- ^equivalence class (only correct for a root) 

instance (Ord a, Show a) => Show (UnionFindTree a) where 
    show (uft @ (UnionFindTree m t c)) =
        show $ map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $
            h uft $ M.keys m
        where
          h uft = 
              fst . foldr (\a (x,t) -> 
                               let (r,t') = find a t
                               in ( (a,r):x, t') ) ([],uft)

    -- > equate "a" "b" $ equate "c" "e" $ equate "d" "c" empty 
    -- [["c","d","e"],["a","b"]]

equals :: Ord a => a -> a -> UnionFindTree a -> (Bool, UnionFindTree a) 
equals x1 x2 t = 
    let (r1,t1) = find x1 t 
        (r2,t2) = find x2 t1 
    in (r1 == r2, t2) 
        
empty :: UnionFindTree a 
empty = UnionFindTree M.empty IM.empty IM.empty 

find :: Ord a => a -> UnionFindTree a -> (Int, UnionFindTree a) 
find a (UnionFindTree m t c) =
    case M.lookup a m of 
      Nothing -> 
          let i   = M.size m 
              m'  = M.insert a i m 
              c'  = IM.insert i (mappend (return a) mempty) c 
              t'  = IM.insert i (-1) t 
          in (i, UnionFindTree m' t' c') 
      Just i -> 
          let (root,t') = findAux i t 
          in (root, UnionFindTree m t' c) 
    where
      findAux i t =
          let p = fromJust $ IM.lookup i t 
          in if p < 0 then 
                 (i,t) 
             else 
                 let (root,t') = findAux p t 
                 in (root, IM.insert i root t')
          
equate :: Ord a => a -> a -> UnionFindTree a -> UnionFindTree a
equate a1 a2 uft = 
    let (root1,uft')  = find a1 uft
        (root2,uft'') = find a2 uft'
        (UnionFindTree m t c) = uft'' 
        rk1 = - (fromJust $ IM.lookup root1 t)
        rk2 = - (fromJust $ IM.lookup root2 t)
    in if root1 == root2 then 
           uft''
       else
           if rk1 < rk2 then 
               insert root1 root2 rk1 rk2 m t c
           else
               insert root2 root1 rk2 rk1 m t c 
    where
      insert r1 r2 rk1 rk2 m t c =
          let t'  = IM.insert r1 (- (rk1 + rk2)) t
              t'' = IM.insert r2 r1 t'
              c1  = fromJust $ IM.lookup r1 c
              c2  = fromJust $ IM.lookup r2 c 
              c'  = IM.insert r1 (mappend c1 c2) c 
          in UnionFindTree m t'' c'

equivalenceClass :: Ord a => a -> UnionFindTree a -> [a] 
equivalenceClass a utf =
    let (i,UnionFindTree _ _ c) = find a utf 
    in Data.Foldable.toList $ fromJust $ IM.lookup i c 