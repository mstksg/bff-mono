{-# OPTIONS_GHC -cpp #-}
{-|
  The module provides counterparts of @..By@ functions in "Data.List"
  for monadic observations. 
-}
module Data.BffMono.Utility 
    ( 
     ifM, nubByM, deleteByM, deleteFirstByM, unionByM, 
     intersectByM, elemByM, groupByM, 
#if __GLASGOW_HASKELL__ < 708
     traceM, 
#endif
     sortByM, insertByM, maximumByM, minimumByM
     ) where 


import Control.Monad 
import Debug.Trace 
    
ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM m x y = m >>= (\b -> if b then x else y)

#if __GLASGOW_HASKELL__  < 708 
traceM :: Monad m => m String -> m a -> m a 
traceM m y = do { x <- m; trace x y }
#endif 

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq = f 
    where
      f []     = return []
      f (x:xs) = do { r <- deleteByM eq x xs
                    ; y <- f r
                    ; return $ x:y }

deleteByM :: Monad m => (a -> a -> m Bool) -> a -> [a] -> m [a]
deleteByM _  _ []     = return []
deleteByM eq x (y:ys) = 
    do { b <- eq x y 
       ; r <- deleteByM eq x ys 
       ; return (if b then r else y:r) }

deleteFirstByM :: Monad m => (a -> a -> m Bool) -> a -> [a] -> m [a]
deleteFirstByM _  _ []     = return []
deleteFirstByM eq x (y:ys) = 
    do { b <- eq x y 
       ; if b then 
             return ys
         else 
             do { r <- deleteFirstByM eq x ys
                ; return $ y:r }}

unionByM :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
unionByM eq xs ys = 
    do { ys' <- foldM (flip (deleteByM eq)) ys xs 
       ; return $ xs ++ ys' }

intersectByM :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
intersectByM eq xs ys = f xs
    where
      f [] = return []
      f (z:zs) = do { b <- elemByM eq z ys
                    ; r <- f zs 
                    ; return (if b then z:r else r) }
      
    
elemByM :: Monad m => (a -> a -> m Bool) -> a -> [a] -> m Bool 
elemByM _  _ []     = return False 
elemByM eq x (y:ys) = 
    do { b <- eq x y 
       ; if b then 
             return True 
         else 
             elemByM eq x ys}

groupByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]] 
groupByM eq = g 
    where
      g []     = return []
      g (x:xs) = f [x] xs 
      f r  [] = return [reverse r] 
      f (y:ys) (x:xs) =
          do { b <- eq x y 
             ; if b then 
                   f (x:y:ys) xs 
               else
                   do { r <- f [x] xs 
                      ; return $ reverse (y:ys):r }}

sortByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM ord zs = ms (map (:[]) zs)
    where
      ms []  = return []
      ms [r] = return r
      ms r   = step r >>= ms 

      step []  = return []
      step [r] = return [r]
      step (xs:ys:rss) = 
          do { xys  <- merge xs ys
             ; rss' <- step rss 
             ; return $ xys : rss' }
    

      merge [] ys = return ys
      merge xs [] = return xs 
      merge (x:xs) (y:ys) = 
          do { o <- ord x y 
             ; case o of 
                 EQ -> liftM ((x:) . (y:)) $ merge xs ys 
                 LT -> liftM (x:) $ merge xs (y:ys)
                 GT -> liftM (y:) $ merge (x:xs) ys }

insertByM :: Monad m => (a -> a -> m Ordering) -> a -> [a] -> m [a]
insertByM ord = f 
    where
      f a [] = return [a]
      f a (x:xs) =
          do { o <- ord a x  
             ; case o of 
                 GT -> liftM (x:) $ f a xs 
                 _  -> return (a:x:xs)}

maximumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m a
maximumByM ord = f 
    where 
      f []     = errorEmptyList "maximumByM"
      f (x:xs) = g x xs 

      g a []     = return a
      g a (x:xs) = 
          do { o <- ord a x
             ; case o of 
                 LT -> g x xs 
                 _  -> g a xs}

minimumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m a
minimumByM ord = f 
    where 
      f []     = errorEmptyList "minimumByM"
      f (x:xs) = g x xs 

      g a []     = return a
      g a (x:xs) = 
          do { o <- ord a x
             ; case o of 
                 GT -> g x xs 
                 _  -> g a xs}

errorEmptyList :: String -> a 
errorEmptyList f = 
    error ("Language.CheapB18n.Utility." ++ f ++ ": empty list")



             
  
                         
    
    
