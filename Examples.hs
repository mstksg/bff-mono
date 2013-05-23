{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts #-}

import Language.CheapB18n 
import Language.CheapB18n.Utility 

import Data.Traversable (Traversable)
import Data.Functor 
import Data.Foldable (Foldable) 

import Control.Monad 

import Debug.Trace 

data Tree a = N a [Tree a] -- polymorphic tree type 
            deriving (Show, Functor, Foldable, Traversable)

label (N x _) = x 

data L = A String -- Attribute 
       | T String -- Text 
       | E String -- Element 
       deriving (Show, Eq, Ord)


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
                 author [last [t "Abiteboul"], first [t "serge"]], 
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
    

(///) :: (Eq c, PackM c a m) => Tree a -> a -> m [Tree a]
(N e ts) /// t = do { cs <- mapM (g t) ts 
                    ; return $ concat cs }
    where
      g t (n@(N e ts)) = do { b <- liftO2 (==) e t 
                            ; if b then 
                                  return [n]
                              else 
                                  return []}

                                 
children :: Tree a -> [Tree a]       
children (N e ts) = ts 
                                  

-- Q1 
q1 :: PackM L a m => Tree a -> m (Tree a)
q1 t = do { books <- t /// (new $ E "book") 
          ; ts' <- mapM h books
          ; return $ N (new $ E "bib") (concat ts') }
    where
      h b = do { yss <- b /// (new $ A "year")
               ; let [y] = concatMap children yss 
               ; t <- b /// (new $ E "title")
               ; pss <- b /// (new $ E "publisher")
               ; let ps = concatMap children pss
               ; case ps of
                   [p] -> 
                       ifM (liftM2 (&&) (liftO2 (>)  (label y) (new $ T "1991"))
                                        (liftO2 (==) (label p) (new $ T "Addision-Wesley")))
                               (return [N (new $ E "book") (N (new $ A "year") [y] : t)] )
                               (return [])
                   _  -> 
                       return []
               }

      

