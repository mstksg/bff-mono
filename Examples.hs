{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             NoMonomorphismRestriction, FlexibleContexts  
  #-}

import Data.BffMono 
import Data.BffMono.Utility 

import Data.Traversable (Traversable)
import Data.Functor 
import Data.Foldable (Foldable) 

import Control.Monad 

-- | nub function 
myNub :: (Eq c, PackM c a m) => [a] -> m [a]
myNub []    = return []
myNub (a:x) = do { y <- myDelete a x
                 ; z <- myNub y 
                 ; return $ a : z }
    where
      myDelete a [] = return []
      myDelete a (b:x) = 
          ifM (a `eqSync` b) 
              (myDelete a x)
              ((b:) <$> myDelete a x)

{-
*Main> fwd myNub "abba"
"ab"
*Main> bwd myNub "abba" "cb"
"cbbc"
-} 

newtype MyPair a = MyPair { runMyPair :: ([a],[a]) }
    deriving (Functor, Foldable, Traversable, Eq) 


-- | Applying nub function to each component of a pair 
myNub2 :: (Eq c1, Eq c2, PackM c1 a1 m, PackM c2 a2 m) => 
          ([a1],[a2]) -> m ([a1],[a2])
myNub2 (x,y) = do { x' <- myNub x
                  ; y' <- myNub y 
                  ; return $ (x',y')}

myNub2F = runMyPair . fwd (fmap MyPair . myNub2 . runMyPair) . MyPair 

myNub2B x y = runMyPair <$> bwd (fmap MyPair . myNub2 . runMyPair) (MyPair x) (MyPair y) 

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

myNub2' :: (Eq c, PackM c a m) => ([a],[a]) -> m ([a],[a]) 
myNub2' (x,y) = do { x' <- myNub x
                   ; y' <- myNub y 
                   ; _  <- myNub (x++y) 
                   ; return $ (x',y')}

myNub2'F = runMyPair . fwd (fmap MyPair . myNub2' . runMyPair) . MyPair 

myNub2'B x y = runMyPair <$> bwd (fmap MyPair . myNub2' . runMyPair) (MyPair x) (MyPair y) 

{-
*Main> myNub2'F ("abba", "bccb")
("ab","bc")
*Main> myNub2'B ("abba","bccb") ("cb", "bc")
*** Exception: user error (Violated Invariants)
*Main> myNub2'B ("abba","bccb") ("ad", "bc")
("adda","dccd")
-}

-- | "count word" example

newtype XIntList a = XIntList { runXIntList :: [(a,Int)] } 
    deriving (Functor, Foldable, Traversable, Eq) 

countWords :: PackM String a m => [a] -> m [(a,Int)] 
countWords = 
    sortByM compareSync 
    >=> groupByM eqSync 
    >=> mapM (\ws -> return (head ws, length ws))

f :: PackM String a m => [a] -> m [a]
f = sortByM compareSync 

countWordsF ws = 
     runXIntList $ fwd (fmap XIntList . countWords) ws 

countWordsB ws ct =
     bwd (fmap XIntList . countWords) ws (XIntList ct)

srcWords = 
    words $
    unlines ["Old MacDonald had a farm E-I-E-I-O"
            , "And on that farm he had a cow E-I-E-I-O"
            , "With a moo moo here and a moo moo there"
            , "Here a moo" ++" "++ "there a moo" ++" "++  "everywhere a moo moo"
            , "Old MacDonald had a farm E-I-E-I-O"]

{-
*Main> countWordsF srcWords 
[("And",1),("E-I-E-I-O",3),("Here",1),("MacDonald",2),("Old",2),("With",1),("a",8),("and",1),("cow",1),("everywhere",1),("farm",3),("had",3),("he",1),("here",1),("moo",8),("on",1),("that",1),("there",2)]
-}

updWordCount = 
    [("And",1),("E-I-E-I-O",3),("Here",1),("McDonald",2),("Old",2),("With",1),("a",8),("and",1),("cow",1),("everywhere",1),("farm",3),("had",3),("he",1),("here",1),("moo",8),("on",1),("that",1),("there",2)]

{-
*Main> countWordsB srcWords updWordCount 
["Old","McDonald","had","a","farm","E-I-E-I-O","And","on","that","farm","he","had","a","cow","E-I-E-I-O","With","a","moo","moo","here","and","a","moo","moo","there","Here","a","moo","there","a","moo","everywhere","a","moo","moo","Old","McDonald","had","a","farm","E-I-E-I-O"]
-}

{-
However, we cannot change "moo" to "baa" or "cow" to "lamb" 
because it changes the order of words. 
-}

-- | "count word" example, again 

countWords' :: PackM String a m => [a] -> m [(a,Int)] 
countWords' []     = return []
countWords' (w:ws) = 
    do { (i,ws') <- deleteAndCount w ws 
       ; ((w,i+1):) <$> countWords' ws' }
    where
      deleteAndCount w [] = return (0,[])
      deleteAndCount w (a:x) = 
          ifM (w `eqSync` a) 
              (do { (i,ws') <- deleteAndCount w x
                  ; return (i+1,ws') })
              (do { (i,ws') <- deleteAndCount w x
                  ; return (i,a:ws') })

countWords'F ws = 
     runXIntList $ fwd (fmap XIntList . countWords') ws 

countWords'B ws ct =
     bwd (fmap XIntList . countWords') ws (XIntList ct)

{-
*Main> countWords'F srcWords 
[("Old",2),("MacDonald",2),("had",3),("a",8),("farm",3),("E-I-E-I-O",3),("And",1),("on",1),("that",1),("he",1),("cow",1),("With",1),("moo",8),("here",1),("and",1),("there",2),("Here",1),("everywhere",1)]
-} 

updWordCount' = 
    [("Old",2),("MacDonald",2),("had",3),("a",8),("farm",3),("E-I-E-I-O",3),("And",1),("on",1),("that",1),("he",1),("lamb",1),("With",1),("bar",8),("here",1),("and",1),("there",2),("Here",1),("everywhere",1)]


{-
*Main> unwords <$> countWords'B srcWords updWordCount'
"Old MacDonald had a farm E-I-E-I-O And on that farm he had a lamb E-I-E-I-O With a bar bar here and a bar bar there Here a bar there a bar everywhere a bar bar Old MacDonald had a farm E-I-E-I-O"
-} 


