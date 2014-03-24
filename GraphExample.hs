{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             NoMonomorphismRestriction, FlexibleContexts  
  #-}

import Data.BffMono 
import Data.BffMono.Utility 

import Data.Traversable (Traversable)
import Data.Functor 
import Data.Foldable (Foldable) 

import Data.List 

import Control.Monad 

newtype Graph a = Graph [(Int, a, Int)]
    deriving (Functor, Foldable, Traversable, Eq) 

instance Show a => Show (Graph a) where 
    show (Graph es) = 
        unlines $ map (\(s,e,d) -> 
                           show s ++ " --[ " ++ show e ++ " ]--> " ++ show d) es

srcGraph = 
    Graph [ (0,"members",1), (1,"mem",2), (1,"mem",8)
          , (2,"name",3), (2,"contact",5), (2,"friend",8)
          , (3,"Matsuda",4), (5,"phone",6), (6,"+81-90-XXXX-YYYY",7)
          , (8,"friend",2), (8,"name",9), (9,"Wang",10)
          , (8,"contact",11), (11,"mail",12),(12,"wmeng@XXX.YYY",13) ]

{-
*Main> srcGraph 
0 --[ "members" ]--> 1
1 --[ "mem" ]--> 2
1 --[ "mem" ]--> 8
2 --[ "name" ]--> 3
2 --[ "contact" ]--> 5
2 --[ "friend" ]--> 8
3 --[ "Matsuda" ]--> 4
5 --[ "phone" ]--> 6
6 --[ "+81-90-XXXX-YYYY" ]--> 7
8 --[ "friend" ]--> 2
8 --[ "name" ]--> 9
9 --[ "Wang" ]--> 10
8 --[ "contact" ]--> 11
11 --[ "mail" ]--> 12
12 --[ "wmeng@XXX.YYY" ]--> 13
-}

m2m :: PackM String a m => Graph a -> m (Graph a)
m2m = rename "mem" "member" 
      >=> rename "friend" "knows"
      >=> contract "contact" 

rename x y (Graph es) = 
    Graph <$> mapM (\(s,e,d) -> 
                        ifM (liftO2 (==) e (new x))
                            (return (s,new y,d))
                            (return (s,e,d))) es 

contract x (Graph es) = 
    do { nodes <- nub <$> concat <$> 
                  mapM (\(s,e,d) -> return [s,d]) es 
       ; conts <- concat <$> 
                  mapM (\(s,e,d) -> 
                            ifM (liftO2 (==) e (new x))
                                (return [(s,d)])
                                (return [])) es 
--       ; let uft = foldr (\(a,b) t -> UF.equate a b t) UF.empty conts 
       ; let rstCls = mkSymTransClosure conts nodes 
       ; let repr x = minimum [ y | (x',y) <- rstCls, x == x' ]
       ; es' <- concat <$> 
                mapM (\(s,e,d) -> 
                          ifM (liftO2 (==) e (new x))
                              (return [])
                              (return [(repr s, e, repr d)])) es
       ; return $ Graph es' }
           where
             mkSymTransClosure rs ns = mkTransClosure $ nub $ rs ++ [ (y,x) | (x,y) <- rs ] ++ [ (x,x) | x <- ns ]
             mkTransClosure rs = 
                 let rs' = nub $ rs ++ [ (x,z) | (x,y) <- rs, (y',z) <- rs, y == y' ]
                 in if rs == rs' then rs else mkTransClosure rs'
             

viewGraph = 
    Graph [ (0,"members",1), (1,"member",2), (1,"member",8)
          , (2,"name",3), (2,"knows",8)
          , (3,"Matsuda",4), (2,"phone",6), (6,"+81-90-XXXX-YYYY",7)
          , (8,"knows",2), (8,"name",9), (9,"Wang",10)
          , (8,"mail",12),(12,"wmeng@XXX.YYY",13) ]
-- equals to fwd m2m srcGraph 
      
{-
*Main> fwd m2m srcGraph 
0 --[ "members" ]--> 1
1 --[ "member" ]--> 2
1 --[ "member" ]--> 8
2 --[ "name" ]--> 3
2 --[ "knows" ]--> 8
3 --[ "Matsuda" ]--> 4
2 --[ "phone" ]--> 6
6 --[ "+81-90-XXXX-YYYY" ]--> 7
8 --[ "knows" ]--> 2
8 --[ "name" ]--> 9
9 --[ "Wang" ]--> 10
8 --[ "mail" ]--> 12
12 --[ "wmeng@XXX.YYY" ]--> 13

*Main> viewGraph 
0 --[ "members" ]--> 1
1 --[ "member" ]--> 2
1 --[ "member" ]--> 8
2 --[ "name" ]--> 3
2 --[ "knows" ]--> 8
3 --[ "Matsuda" ]--> 4
2 --[ "phone" ]--> 6
6 --[ "+81-90-XXXX-YYYY" ]--> 7
8 --[ "knows" ]--> 2
8 --[ "name" ]--> 9
9 --[ "Wang" ]--> 10
8 --[ "mail" ]--> 12
12 --[ "wmeng@XXX.YYY" ]--> 13

-} 

updGraph1 = 
    Graph [ (0,"members",1), (1,"member",2), (1,"member",8)
          , (2,"name",3), (2,"knows",8)
          , (3,"Matsuda",4), (2,"mail",6), (6,"kztk@YYY.ZZZ",7)
          , (8,"knows",2), (8,"name",9), (9,"Wang",10)
          , (8,"mail",12),(12,"wmeng@XXX.YYY",13) ]

{-
*Main> updGraph1 
0 --[ "members" ]--> 1
1 --[ "member" ]--> 2
1 --[ "member" ]--> 8
2 --[ "name" ]--> 3
2 --[ "knows" ]--> 8
3 --[ "Matsuda" ]--> 4
2 --[ "mail" ]--> 6           -- Updated!
6 --[ "kztk@YYY.ZZZ" ]--> 7   -- Updated!
8 --[ "knows" ]--> 2
8 --[ "name" ]--> 9
9 --[ "Wang" ]--> 10
8 --[ "mail" ]--> 12
12 --[ "wmeng@XXX.YYY" ]--> 13

*Main> bwd m2m srcGraph updGraph1 
0 --[ "members" ]--> 1
1 --[ "mem" ]--> 2
1 --[ "mem" ]--> 8
2 --[ "name" ]--> 3
2 --[ "contact" ]--> 5
2 --[ "friend" ]--> 8
3 --[ "Matsuda" ]--> 4
5 --[ "mail" ]--> 6          -- Updated!
6 --[ "kztk@YYY.ZZZ" ]--> 7  -- Updated!
8 --[ "friend" ]--> 2
8 --[ "name" ]--> 9
9 --[ "Wang" ]--> 10
8 --[ "contact" ]--> 11
11 --[ "mail" ]--> 12
12 --[ "wmeng@XXX.YYY" ]--> 13
-}
