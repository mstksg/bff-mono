{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             NoMonomorphismRestriction  
  #-}

import Language.CheapB18n 
import Language.CheapB18n.Utility 

import Data.Traversable (Traversable)
import Data.Functor 
import Data.Foldable (Foldable) 



-- | nub function 
-- myNub :: (Eq c, PackM c a m) => [a] -> m [a]
myNub []    = return []
myNub (a:x) = do { y <- myDelete a x
                 ; z <- myNub y 
                 ; return $ a : z }
    where
      myDelete a [] = return []
      myDelete a (b:x) = 
          ifM (a `eqSync` b) 
              (myDelete a x)
              (fmap (b:) $ myDelete a x)

{-
*Main> fwd myNub "abba"
"ab"
*Main> bwd myNub "abba" "cb"
"cbbc"
-} 

newtype MyPair a = MyPair { runMyPair :: ([a],[a]) }
    deriving (Functor, Foldable, Traversable, Eq) 


-- | Applying nub function to each component of a pair 
myNub2 (MyPair (x,y)) = do { x' <- myNub x
                           ; y' <- myNub y 
                           ; return $ MyPair (x',y')}

-- NoMonomorphismResctriction is required to type check @myNub2F@ 
-- w/o type annotation 
myNub2F = runMyPair . fwd myNub2 . MyPair 

myNub2B x y = runMyPair <$> bwd myNub2 (MyPair x) (MyPair y) 

{-
*Main> myNub2F ("abba", "bccb")
("ab","bc")
*Main> myNub2B ("abba","bccb") ("cb", "bc")
("cbbc","bccb")
*Main> myNub2B ("abba","bccb") ("ad", "bc")
("adda","bccb")
-}


-- | A variant of @myNub2@, which has the same behavior to the 
--   original BFF approach
myNub2' (MyPair (x,y)) = do { x' <- myNub x
                            ; y' <- myNub y 
                            ; _  <- myNub (x++y) 
                            ; return $ MyPair (x',y')}

myNub2'F = runMyPair . fwd myNub2' . MyPair 

myNub2'B x y = runMyPair <$> bwd myNub2' (MyPair x) (MyPair y) 

{-
*Main> myNub2'F ("abba", "bccb")
("ab","bc")
*Main> myNub2'B ("abba","bccb") ("cb", "bc")
*** Exception: user error (Violated Invariants)
*Main> myNub2'B ("abba","bccb") ("ad", "bc")
("adda","dccd")
-}


