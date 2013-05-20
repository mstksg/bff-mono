module Language.CheapB18n.Utility where 

import Control.Monad 
    
ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM m x y = m >>= (\b -> if b then x else y)

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq xs = f xs 
    where
      f []     = return $ []
      f (x:xs) = do { r <- deleteByM eq x xs
                    ; return $ x:r }

deleteByM :: Monad m => (a -> a -> m Bool) -> a -> [a] -> m [a]
deleteByM eq x [] = return []
deleteByM eq x (y:ys) = 
    do { b <- eq x y 
       ; r <- deleteByM eq x ys 
       ; if b then 
             return r 
         else
             return $ x:r }

deleteFirstByM :: Monad m => (a -> a -> m Bool) -> a -> [a] -> m [a]
deleteFirstByM eq x [] = return []
deleteFirstByM eq x (y:ys) = 
    do { b <- eq x y 
       ; if b then 
             return ys
         else 
             do { r <- deleteFirstByM eq x ys
                ; return $ x:r }}

unionByM :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
unionByM eq xs ys = 
    do { ys' <- f xs ys 
       ; return $ xs ++ ys' }
    where
      f [] ys = return ys 
      f (x:xs) ys = do { ys' <- deleteByM eq x ys 
                       ; f xs ys' }

intersectByM :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
intersectByM eq xs ys = f xs
    where
      f [] = return []
      f (x:xs) = do { b <- elemByM eq x ys
                    ; r <- f xs 
                    ; if b then 
                          return $ x:r
                      else
                          return r }
      
    
elemByM :: Monad m => (a -> a -> m Bool) -> a -> [a] -> m Bool 
elemByM eq x [] = return False 
elemByM eq x (y:ys) = 
    do { b <- eq x y 
       ; if b then 
             return True 
         else 
             elemByM eq x ys}

groupByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]] 
groupByM eq xs = g xs 
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
sortByM ord xs = ms (map (\x -> [x]) xs)
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
                 EQ -> liftM (x:) $  merge xs ys 
                 LT -> liftM (x:) $ merge xs (y:ys)
                 GT -> liftM (y:) $ merge (x:xs) ys }

insertBy :: Monad m => (a -> a -> m Ordering) -> a -> [a] -> m [a]
insertBy ord a xs = f a xs 
    where
      f a [] = return [a]
      f a (x:xs) =
          do { o <- ord a x  
             ; case o of 
                 GT -> liftM (x:) $ f a xs 
                 _  -> return $ (a:x:xs)}

maximumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m a
maximumByM ord xs = f xs 
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
minimumByM ord xs = f xs 
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



             
  
                         
    
    