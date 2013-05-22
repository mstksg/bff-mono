{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Language.CheapB18n 

import Language.CheapB18n.Base 
import Language.CheapB18n.CheckHistory 
import Language.CheapB18n.Utility 

import Test.QuickCheck 

import Data.Function 
import Data.List 
import Data.Functor
import Data.Foldable hiding (concat, concatMap, mapM_, all, elem)
import Data.Traversable hiding (mapM)
import qualified Data.Traversable as T (mapM) 

import Control.Applicative ((<$>))
import Control.Monad 
import Control.Monad.Writer 
import Debug.Trace 

import Text.Printf 
import qualified Data.IntMap as I 

import System.Exit 

main = mapM_ (\(s,a) -> printf "%-20s: " s >> a) tests
       >> exitSuccess 

tests = [ ("assignIDs",         quickCheck prop_assignIDs)
        , ("update/empty",      quickCheck prop_updateEmpty )
        , ("matchView-Correct", quickCheck prop_matchViewCorrect )
        , ("matchView-Min",     quickCheck prop_matchViewMin )
        , ("checkHistoryUnit",  quickCheck prop_checkHistoryUnit )
        , ("update/new",        quickCheck prop_updateNew )
          --- the following three comes from free theorem 
        , ("getput",            quickCheck prop_getput)
        , ("putget",            quickCheck prop_putget)
        , ("assignIDs/Free",    quickCheck prop_assignIDsFree )
        ]

prop_assignIDs s = fmap body (assignIDs s) == s 
    where
      _ = s :: Tree Int 

prop_updateEmpty s = update I.empty s == s
    where
      _ = s :: Loc Int 

prop_assignIDsFree s =
    all (\y -> y `elem` xs) ys 
    where
      (xview, hist) = runWriter $ links (assignIDs s)
      _ = checkHistory (update I.empty) hist 
      ys = concatMap f $ Data.Foldable.toList $ xview 
      xs = concatMap f $ Data.Foldable.toList $ assignIDs s 
      f (Loc x InTrans) = []
      f (Loc x (InSource i)) = [(i,x)]
                        
         

inverseUpdate v n = 
    do xs <- liftM I.fromList $ mapM (\i -> do { y <- arbitrary ; return (i,y)}) [1..n]
       f xs v 
    where
      f xs (Node x ys) = 
          do { j  <- choose (1,2*n)
             ; n' <- case I.lookup j xs of 
                       Just y  -> return $ Loc y (InSource j)
                       Nothing -> return $ Loc x InTrans 
             ; ys' <- mapM (f xs) ys 
             ; return $ Node n' ys' }


prop_matchViewCorrect v = 
    do { xview <- inverseUpdate v 200
       ; case matchViews xview v of 
           Left  s -> let _ = s :: String in return True
           Right upd ->
               if isCorrectView xview && I.size upd >= 0 then
                   return $ fmap (body . update upd) xview == v
               else 
                   return False}
    where
      _ = v :: Tree Int 
      
-- Checks if |xview| does not contain any two elements |Loc a (InSource i)| |Loc b (InSource j)| 
-- such that |i == j| but |a /= b|.

isCorrectView xview =
    all (\x -> length (nubBy ((==) `on` snd) x) == 1) $ 
      groupBy ((==) `on` fst) $ 
        sortBy (compare `on` fst) $ 
           concatMap (\(Loc x y) -> case y of 
                                      InTrans -> []
                                      InSource i -> [(i,x)]) $
               Data.Foldable.toList xview

prop_matchViewMin v = 
    do { xview <- inverseUpdate v 200
       ; if isCorrectView xview then 
             return $ either error id (matchViews xview (fmap body xview)) == I.empty 
         else
             return False }
    where
      _ = v :: Tree Int

      


data Check a = Check a 
instance Show (Check a) where
    show _ = "<fun>"

instance Arbitrary a => Arbitrary (Check a) where
    arbitrary = liftM Check arbitrary 

prop_checkHistoryUnit (Check check,ul,xs,y) =
    if checkHistory u (return $ CheckResult check xs y) == True then 
        check (map u xs) == y 
    else
        True 
    where
      u i = case I.lookup i um of 
              Just j  -> j 
              Nothing -> i 
      um = I.fromList ul 
      _ = xs :: [Int]
      _ = y  :: Int 

prop_updateNew (x,ul) =
    update upd y == y
    where
      upd = I.fromList ul
      y   = new x :: Loc Int 


-- | Tree with node type a 
data Tree a = Node a [Tree a]
            deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Arbitrary a => Arbitrary (Tree a) where 
    arbitrary = sized (\i -> mkTree i) 
        where
          mkTree 0 = do { x <- arbitrary 
                        ; return $ Node x [] }
          mkTree 1 = mkTree 0 
          mkTree i = 
              do { x <- arbitrary 
                 ; liftM (Node x) (mkTrees i) }
          mkTrees 0 = return []
          mkTrees i =
              do { j <- choose (1, max 1 (i `div` 10) )
                 ; t  <- mkTree  j 
                 ; ts <- mkTrees (i - j)
                 ; return $ t:ts }

    shrink (Node x []) = []
    shrink (Node x (y:xs)) =
        [ Node x' xs | x' <- shrink x ]
        ++
        [ Node x' (y':xs') | y' <- shrink y, Node x' xs' <- shrink (Node x xs) ]

instance Arbitrary Index where 
    arbitrary = do { v <- arbitrary 
                   ; case v of 
                       Nothing -> return InTrans
                       Just x  -> return $ InSource x }

instance Arbitrary a => Arbitrary (Loc a) where 
    arbitrary = liftM2 Loc arbitrary arbitrary 


-- | An atomic datatype we want to abstract.
--   We assume that the only observation is to 
--   check its equality. 
-- 
--   The type |Label| abstracts the XML elements and PCData. 
data Label = LN String | LText String
           deriving (Eq,Ord,Show)


instance Arbitrary Label where 
    arbitrary = oneof [ liftM LN arbitrary, liftM LText arbitrary ]
    shrink (LN s)    = [LN s' | s' <- shrink s ] ++ [LText s]
    shrink (LText s) = [f  s' | f <- [LN,LText], s' <- shrink s ]

-- It is not so much easy to avoid type declrations in the following 
-- defintions. 

links t = Node (new $ LN "root") <$> linkss t
    
linkss (Node a ts) =
    ifM (liftO2 (==) a (new $ LN "a"))
            (return [Node a ts])
            (concat <$> mapM linkss ts)

linksF s = fwd links s 

linksB s v = either error id (bwd links s v)

----------------------------------------
-- A sample source 
test_src = 
    Node (LN "b") [
      Node (LN "a") [
        Node (LN "@href") [
          Node (LText "www") []
        ], 
        Node (LText "link text") []
      ]
    ]

-- Updated view 1 
test_view1 = 
    Node (LN "root") [
      Node (LN "a") [
        Node (LN "@href") [
          Node (LText "www2") []
        ], 
        Node (LText "link text") []
      ]
    ]

-- Updated view 2 
test_view2 = 
    Node (LN "root") [
      Node (LN "a") [
        Node (LN "@href") [
          Node (LText "www2") []
        ], 
        Node (LText "linked text") []
      ]
    ]

-- Updated view 3: put must fail for the view 
test_view3 = 
    Node (LN "root") [
      Node (LN "A") [
        Node (LN "@href") [
          Node (LText "www") []
        ], 
        Node (LText "link text") []
      ]
    ]

-- Updated view 4: put must fail for the view 
test_view4 = 
    Node (LN "Root") [
      Node (LN "a") [
        Node (LN "@href") [
          Node (LText "www") []
        ], 
        Node (LText "link text") []
      ]
    ]

prop_getput s = 
    case bwd links s (fwd links s) of 
      Left  s  -> let _ = s :: String in False 
      Right s' -> s == s' 

prop_putget s =
    do { v <- T.mapM (\_ -> arbitrary) s
       ; case bwd links s v of 
           Left  s  -> let _ = s :: String in return True 
           Right s' -> return $ fwd links s == v }

