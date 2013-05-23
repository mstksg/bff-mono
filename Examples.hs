{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts #-}

import Language.CheapB18n 
import Language.CheapB18n.Utility 

import Data.Traversable (Traversable)
import Data.Functor 
import Data.Foldable (Foldable) 

import Data.Function (on)
import Data.List 

import Control.Monad 
import Control.Monad.List 

import Debug.Trace 

import Text.PrettyPrint.HughesPJ

data Tree a = N a [Tree a] -- polymorphic tree type 
            deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

label (N x _) = x 

data L = A String -- Attribute 
       | T String -- Text 
       | E String -- Element 
       deriving (Show, Eq, Ord)


pretty (N (T t) []) = text t 
pretty (N (E t) ts) = cat [text "<" <> text t <+> 
                              hsep (map pretty attrs) <> text ">",
                           nest 4 $ cat (map pretty others), 
                           text "</" <> text t <> text ">"]
    where
      (attrs,others) = partition p ts 
          where p (N (A _) _) = True 
                p _           = False
pretty (N (A a) [N (T t) []]) = text a <> text "=" <> text (show t) 


-- from XML Query Use Cases, 1.1.2 Sample Data 
-- http://www.w3.org/TR/xquery-use-cases/#xmp-data
test_src
    = bib [
       book "1994" [
                 title [t "TCP/IP illustrated"], 
                 author [last [t "Stevens"], first [t "W."]], 
                 publisher [t "Addision-Wesley"],
                 price     [t "65.95"]
                ], 
       book "1992" [
                 title [t "Advanced Programming in the Unix environment"], 
                 author [last [t "Stevens"], first [t "W."]], 
                 publisher [t "Addision-Wesley"],
                 price     [t "65.95"]
                ], 
       book "2000" [
                 title [t "Data on the Web"], 
                 author [last [t "Abiteboul"], first [t "Serge"]], 
                 author [last [t "Buneman"], first [t "Peter"]],
                 author [last [t "Suciu"], first [t "Dan"]],
                 publisher [t "Morgan Kaufmann Publishers"],
                 price     [t "39.95"]
                ],
       book "1999" [
                 title [t "The Economics of Technology and Content for Digital TV"],
                 editor [last [t "Gerbarg"], first [t "Darcy"], affil [t "CITI"]],
                 publisher [t "Kluwer Academic Publishers"],
                 price [t "129.95"]                              
                ]
      ]
    where 
      t x       = N (T x) []
      bib       = N (E "bib") 
      book y x  = N (E "book") ([N (A "year") [N (T y) []]]++x)
      title     = N (E "title")
      author    = N (E "author")
      publisher = N (E "publisher")
      price     = N (E "price")
      last      = N (E "last")
      first     = N (E "first")
      affil     = N (E "affiliation") 
      editor    = N (E "editor")
    

liftO'  p x    = ListT $ fmap (:[]) $ liftO p x 
liftO1' p x    = ListT $ fmap (:[]) $ liftO1 p x 
liftO2' p x y  = ListT $ fmap (:[]) $ liftO2 p x y 

withLabel :: (Eq c, PackM c a m) => a -> Tree a -> ListT m (Tree a)
withLabel t (N e ts) = p $ map (g t) ts 
    where
      p []     = mzero 
      p (x:xs) = x `mplus` p xs 
      g t (n@(N e ts)) = do { assertM $ liftO2' (==) e t
                            ; return n }


findAll t (n@(N e ts))
    = do { b <- liftO2' (==) e t 
         ; (if b then return n else mzero) `mplus` findAlls t ts}
    where
      findAlls t ts = foldr mplus mzero $ map (findAll t) ts 
                                 
children :: Monad m => Tree a -> ListT m (Tree a)
children (N e ts) = ListT $ mapM return ts 

       
gather :: Monad m => ListT m a -> ListT m [a]
gather (ListT x) = 
    ListT $ do { a <- x 
               ; return $ [a] }
                   
pick :: Monad m => ListT m a -> m a 
pick (ListT x) = do { a <- x
                    ; return $ head a }
       
assertM x = do { b <- x 
               ; if b then return () else fail "..."}

-- Q1 
q1 t = pick $ 
       do { bs <- gather $ withLabel (new $ E "book") t >>= h
          ; return $ N (new $ E "bib") bs }
    where
      h b = do { y  <- withLabel (new $ A "year") b >>= children 
               ; ts <- gather $ withLabel (new $ E "title") b
               ; p  <- withLabel (new $ E "publisher") b >>= children 
               ; assertM $ liftO2' ((>) `on` g) (label y) (new $ T "1991")
               ; assertM $ liftO2' (==) (label p) (new $ T "Addision-Wesley")
               ; return $ N (new $ E "book") (N (new $ A "year") [y] : ts) } 
          where g (T t) = read t :: Int  

test_view_q1 
    = N (E "bib") [N (E "book") [N (A "year") [N (T "1994") []],
                                 N (E "title") [N (T "TCP/IP illustrated") []]],
                   N (E "book") [N (A "year") [N (T "1992") []],
                                 N (E "title") [N (T "Advanced Programming in the Unix Environment") []]]]

test_view_q1' 
    = N (E "bib") [N (E "book") [N (A "year") [N (T "1994") []],
                                 N (E "title") [N (T "TCP/IP illustrated") []]],
                   N (E "book") [N (A "year") [N (T "1991") []],
                                 N (E "title") [N (T "Advanced Programming in the Unix Environment") []]]]


-- Q2 
q2 t = pick $
       do { ts <- gather $ do { b <- withLabel (new $ E "book") t 
                              ; t <- withLabel (new $ E "title") b
                              ; a <- withLabel (new $ E "author") b
                              ; return $ N (new $ E "result") [t,a] }
          ; return $ N (new $ E "results") ts }


-- Q3 
q3 t = pick $ 
       do { ts <- gather $ do { b  <- withLabel (new $ E "book") t 
                              ; ts <- gather $ withLabel (new $ E "title") b 
                              ; as <- gather $ withLabel (new $ E "author") b 
                              ; return $ N (new $ E "result") (ts++as) }
          ; return $ N (new $ E "results") ts }

-- Q4 
q4 t = pick $ do { ts <- gather $ 
                         do { (last, first) <- lf
                            ; ts <- gather $ 
                                    do { b <- withLabel (new $ E "book") t 
                                       ; a <- withLabel (new $ E "author") b
                                       ; f <- withLabel (new $ E "first" ) a
                                       ; l <- withLabel (new $ E "last")   a
                                       ; assertM $ ListT $ fmap (:[]) $  eq f first 
                                       ; assertM $ ListT $ fmap (:[]) $  eq l last 
                                       ; withLabel (new $ E "title") b}
                            ; return $ N (new $ E "result") 
                                         ([N (new $ E "author") [
                                                 N (new $ E"last") [last],
                                                   N (new $ E"first") [first]]]
                                          ++ ts )}
                 ; return $ N (new $ E "results") ts } 
    where
      eq (N e ts) (N e' ts') = 
          do { b <- liftO2 (==) e e' 
             ; if b then eq' ts ts' else return False }
      eq' [] [] = return True 
      eq' [] _  = return False 
      eq' _ []  = return False 
      eq' (t:ts) (t':ts') =
          do { b <- eq t t' 
             ; if b then eq' ts ts' else return False }
      lf = do { lf <- gather $ 
                      do { as    <- gather $ findAll (new $ E "author") t
                         ; lasts <- gather $ do { a <- ListT $ return as
                                                ; withLabel (new $ E "last") a}
                         ; last  <- ListT $
                                     nubByM eq lasts
                         ; firsts <- gather $ do { a <- ListT $ return as 
                                                 ; f <- withLabel (new $ E "first") a
                                                 ; l <- withLabel (new $ E "last") a
                                                 ; assertM $ ListT $ fmap (:[]) $ eq l last 
                                                 ; return f }
                         ; first  <- ListT $ 
                                       nubByM eq firsts 
                         ; return (last, first) }
              ; ListT $ sortByM comp lf }
          where
            comp (a,b) (c,d) =
                do { o <- cp a c 
                   ; case o of
                       LT -> return LT 
                       GT -> return GT 
                       EQ -> cp b d }
            cp (N e ts) (N e' ts') =
                do { o <- liftO2 compare e e' 
                   ; case o of 
                       LT -> return LT 
                       GT -> return GT 
                       EQ -> cp' ts ts'}
            cp' [] [] = return EQ
            cp' [] _ = return LT 
            cp' _ [] = return GT 
            cp' (t:ts) (t':ts') =
                do { o <- cp t t' 
                   ; case o of 
                       LT -> return LT 
                       GT -> return GT 
                       EQ -> cp' ts ts' }
