{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts #-}

import Language.CheapB18n 
import Language.CheapB18n.Utility 

import Data.Traversable (Traversable)
import Data.Functor 
import Data.Foldable (Foldable) 
import qualified Data.Foldable as Foldable 

import Data.Function (on)
import Data.List 

import Control.Monad 
import Control.Monad.List 

import Control.Monad.Trans 

import Debug.Trace 

import Text.PrettyPrint.HughesPJ

data Tree a = N a [Tree a] -- polymorphic tree type 
            deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

liftEq :: (PackM c a m, Functor t, Foldable t, Eq (t ())) => 
          (c -> c -> Bool) -> t a -> t a -> m Bool 
liftEq eq t1 t2 = 
    if void t1 == void t2 then 
        Foldable.foldrM g True $ zip (Foldable.toList t1) (Foldable.toList t2)
    else 
        return False 
    where
      g (a1,a2) b = if b then liftO2 eq a1 a2 else return False 

type BFilter a m = Tree a -> ListT m (Tree a) 

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
                 title [t "TCP/IP Illustrated"], 
                 author [last [t "Stevens"], first [t "W."]], 
                 publisher [t "Addison-Wesley"],
                 price     [t "65.95"]
                ], 
       book "1992" [
                 title [t "Advanced Programming in the Unix environment"], 
                 author [last [t "Stevens"], first [t "W."]], 
                 publisher [t "Addison-Wesley"],
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
    
-- for Q5 
test_src2 = reviews [ entry [ title [t "Data on the Web"]
                            , price [t "34.95"]
                            , review [t "..."]]
                    , entry [ title [t "Advanced Programming in the Unix environment"]
                            , price [t "65.95"]
                            , review [t "..."]]
                    , entry [ title [t "TCP/IP Illustrated"]
                            , price [t "65.95"]
                            , review [t "..."]]
                    ]
    where
      t x       = N (T x) []
      price     = N (E "price")
      title     = N (E "title")
      review    = N (E "review")
      entry     = N (E "entry")
      reviews   = N (E "reviews")
      


ofLabel :: (Eq c, PackM c a m) => a -> BFilter a m 
ofLabel l t = do { b <- lift $ liftO2 (==) l (label t)
                 ; if b then return t else mzero }

labelWith :: (Eq c, PackM c a m) => (c -> Bool) -> BFilter a m 
labelWith p t = do { b <- lift $ liftO1 p (label t)
                   ; if b then return t else mzero }

children :: Monad m => BFilter a m 
children (N e ts) = ListT $ return ts 

childrenWith f = children >=> f 



union :: Monad m => BFilter a m -> BFilter a m -> BFilter a m 
union f1 f2 t = f1 t `mplus` f2 t 

deepest f (t@(N l ts)) = do { ck <- gather $ f t 
                            ; case ck of 
                                [] -> msum $ map (deep f) ts 
                                _  -> do { let rs = map (deep f) ts 
                                         ; msum $ (return t):rs }}


deep f t = bfs [t] [] 
    where
      bfs [] [] = mzero 
      bfs [] qs = bfs (reverse qs) [] 
      bfs (t@(N l ts):rest) qs = do { ck <- gather $ f t 
                                    ; case ck of 
                                        [] -> bfs rest (reverse ts ++ qs)
                                        _  -> mplus (return t) (bfs rest qs) }
                                              

multi f t = bfs [t] [] 
    where
      bfs [] [] = mzero 
      bfs [] qs = bfs (reverse qs) [] 
      bfs (t@(N l ts):rest) qs = do { ck <- gather $ f t 
                                    ; case ck of 
                                        [] -> bfs rest (reverse ts ++ qs)
                                        _  -> mplus (return t) (bfs rest (reverse ts ++ qs)) }
                                              

       
gather :: Monad m => ListT m a -> ListT m [a]
gather (ListT x) = 
    ListT $ do { a <- x 
               ; return $ [a] }
                   
pick :: Monad m => ListT m a -> m a 
pick (ListT x) = do { a <- x
                    ; return $ head a }

isOk :: Monad m => BFilter a m -> (Tree a -> ListT m Bool)
isOk f t = do { ck <- gather $ f t 
              ; return $ not (null ck) }

guardM :: MonadPlus m => m Bool -> m ()        
guardM x = x >>= guard 

attr :: Pack L a => String -> a 
attr = new . A 
el :: Pack L a => String -> a 
el   = new . E
txt :: Pack L a => String -> a 
txt  = new . T

f /> g = f >=> children >=> g 

keep :: Monad m => BFilter a m 
keep = return 

childrenOfLabel l = childrenWith (ofLabel l) 


-- Q1 
q1 t = pick $ 
       do { bs <- gather $ (keep /> (ofLabel (new $ E "book") >=> h)) t
          ; return $ N (new $ E "bib") bs }
    where
      h b = do { y  <- (keep /> ofLabel (new $ A "year") /> keep) b -- childrenWith (ofLabel (new $ A "year")) b >>= children 
               ; t  <- (keep /> ofLabel (new $ E "title")) b -- childrenWith (ofLabel (new $ E "title")) b
               ; p  <- (keep /> ofLabel (new $ E "publisher") /> keep) b -- childrenWith (ofLabel (new $ E "publisher")) b >>= children 
               ; guardM $ lift $ liftO2 ((>) `on` g) (label y) (new $ T "1991")
               ; guardM $ lift $ liftO2 (==) (label p) (new $ T "Addison-Wesley")
               ; return $ N (new $ E "book") [N (new $ A "year") [y], t] } 
          where g (T t) = read t :: Int  

test_view_q1 
    = N (E "bib") [N (E "book") [N (A "year") [N (T "1994") []],
                                 N (E "title") [N (T "TCP/IP Illustrated (Second Edition)") []]],
                   N (E "book") [N (A "year") [N (T "1992") []],
                                 N (E "title") [N (T "Advanced Programming in the Unix Environment") []]]]

test_view_q1' 
    = N (E "bib") [N (E "book") [N (A "year") [N (T "1994") []],
                                 N (E "title") [N (T "TCP/IP illustrated") []]],
                   N (E "book") [N (A "year") [N (T "1991") []],
                                 N (E "title") [N (T "Advanced Programming in the Unix Environment") []]]]


-- Q2 
q2 t = pick $
       do { ts <- gather $ do { b <- childrenOfLabel (new $ E "book") t 
                              ; t <- childrenOfLabel (new $ E "title") b
                              ; a <- childrenOfLabel (new $ E "author") b
                              ; return $ N (new $ E "result") [t,a] }
          ; return $ N (new $ E "results") ts }


-- Q3 
q3 t = pick $ 
       do { ts <- gather $ do { b  <- childrenOfLabel (new $ E "book") t 
                              ; ts <- gather $ childrenOfLabel (new $ E "title") b 
                              ; as <- gather $ childrenOfLabel (new $ E "author") b 
                              ; return $ N (new $ E "result") (ts++as) }
          ; return $ N (new $ E "results") ts }

-- Q4 
q4 t = pick $ do { ts <- gather $ 
                         do { (last, first) <- lf
                            ; ts <- gather $ 
                                    do { b <- childrenOfLabel (new $ E "book") t 
                                       ; a <- childrenOfLabel (new $ E "author") b
                                       ; f <- childrenOfLabel (new $ E "first" ) a
                                       ; l <- childrenOfLabel (new $ E "last")   a
                                       ; guardM $ lift $ liftEq (==) f first 
                                       ; guardM $ lift $ liftEq (==) l last 
                                       ; childrenOfLabel (new $ E "title") b}
                            ; return $ N (new $ E "result") 
                                         ([N (new $ E "author") [
                                                 N (new $ E"last") [last],
                                                   N (new $ E"first") [first]]]
                                          ++ ts )}
                 ; return $ N (new $ E "results") ts } 
    where
      lf = do { lf <- gather $ 
                      do { as    <- gather $ deep (ofLabel (new $ E "author")) t
                         ; lasts <- gather $ do { a <- ListT $ return as
                                                ; childrenOfLabel (new $ E "last") a}
                         ; last  <- ListT $
                                     nubByM (liftEq (==)) lasts
                         ; firsts <- gather $ do { a <- ListT $ return as 
                                                 ; f <- childrenOfLabel (new $ E "first") a
                                                 ; l <- childrenOfLabel (new $ E "last") a
                                                 ; guardM $ lift $ liftEq (==) l last 
                                                 ; return f }
                         ; first  <- ListT $ 
                                       nubByM (liftEq (==)) firsts 
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


-- Q5 
q5 t1 t2 = pick $ do { bs <- gather $ do { b  <- deep (ofLabel (el "book")) t1 
                                         ; a  <- deep (ofLabel (el "entry")) t2 
                                         ; tb <- childrenOfLabel (el "title") b 
                                         ; ta <- childrenOfLabel (el "title") a
                                         ; guardM $ lift $ liftEq (==) tb ta
                                         ; p1 <- childrenOfLabel (el "price") b >>= children 
                                         ; p2 <- childrenOfLabel (el "price") a >>= children 
                                         ; return $ N (el "book-with-price") 
                                                      [ta,
                                                         N (el "price-bstore2") [p1],
                                                           N (el "price-bstore1") [p2]] }
                     ; return $ N (el "books-with-prices") bs }


data MyPair a = MyPair (Tree a) (Tree a) deriving (Traversable, Functor, Foldable, Eq)

-- pretty $ fwd (\(MyPair a b) -> q5 a b) (MyPair test_src test_src2 )


-- Q6 
q6 t = pick $ do { bs <- gather 
                         $ do { b  <- deep (ofLabel (el "book")) t 
                              ; as <- gather $ childrenOfLabel (el "author") b
                              ; guard (length as > 0)
                              ; t  <- childrenOfLabel (el "title") b 
                              ; return $ N (el "book") 
                                           ([t] ++ take 2 as ++ 
                                            (if length as >= 3 then 
                                                 [N (el "et-al") []]
                                             else
                                                 []))}
                 ; return $ N (el "bib") bs }

-- Q7 
q7 t = pick $ do { bs <- gather 
                         $ do { b <- deep (ofLabel (el "book")) t 
                              ; p <- childrenOfLabel (el "publisher") b >>= children
                              ; y <- childrenOfLabel (attr "year") b >>= children 
                              ; guardM $ lift $ liftO2 (==) (label p) (txt "Addison-Wesley")
                              ; guardM $ lift $ liftO2 ((>) `on` g) (label y) (txt "1991")
                              ; return b }
                 ; bs' <- sortByM cmpTitle bs 
                 ; bs'' <- gather $ do { b <- ListT $ return bs'  
                                       ; y <- childrenOfLabel (attr "year") b
                                       ; t <- childrenOfLabel (el   "title") b
                                       ; return $ N (el "book") [y,t] }
                 ; return $ N (el "bib") bs'' }
    where
      g (T t) = read t :: Int  
      cmpTitle :: PackM L a m => Tree a -> Tree a -> ListT m Ordering 
      cmpTitle b1 b2 =
          do { t1 <- childrenOfLabel (el "title") b1 >>= children 
             ; t2 <- childrenOfLabel (el "title") b2 >>= children 
             ; lift $ liftO2 compare (label t1) (label t2) }
                                                  
-- Q8
q8 t = pick $ do { b <- deep (ofLabel (el "book")) t 
                 ; e <- orEnded b 
                 ; c <- children e
                 ; guardM $ isOk containSuciu c
                 ; t <- childrenOfLabel (el "title") b 
                 ; return $ N (el "book") [t,e] }
    where
      orEnded = deep (labelWith orEndedL) 
          -- do { b <- liftO1' orEndedL e
          --    ; if b then return (N e ts) else (msum $ map orEnded ts) }
          where
            orEndedL (E t) = (reverse . take 2 .reverse) t == "or"
            orEndedL _     = False
      containSuciu = deep (labelWith containSuciuL)
          -- do { b <- liftO1' containSuciuL e
          --    ; if b then return True else fmap or $ mapM containSuciu ts }
          where
            containSuciuL (T t) = "Suciu" `isInfixOf` t 
            containSuciuL _     = False 


test_src_q9 = 
    chapter [ title [ t "Data Model"]
            , section [ title [ t "Syntax For Data Model" ]] 
            , section [ title [ t "XML" ]
                      , section [ title [ t "Basic Syntax" ]]
                      , section [ title [ t "XML and Semistructured Data" ]]]]
   where
      t x       = N (T x) []
      chapter   = N (E "chapter")
      title     = N (E "title")
      section   = N (E "section") 

-- Q9 
q9 t = pick 
       $ do { ts <- gather $ do { cs <- multi (Main.union (ofLabel (el "chapter")) (ofLabel (el "section"))) t  -- findSC t 
                                ; ti <- childrenOfLabel (el "title") cs 
                                ; tt <- children ti 
                                ; guardM $ containXML (label tt)
                                ; return ti }
            ; return $ N (el "results") ts }
    where
      containXML l = lift $ liftO1 (\x -> case x of 
                                            T x -> "XML" `isInfixOf` x
                                            _   -> False) l 
          

test_src_q10
    = prices [
        book [ title [t "Advanced Programming in the Unix environment"]
             , source [t "bstore2.example.com"]
             , price [t "65.95"]
             ]
      , book [ title [t "Advanced Programming in the Unix environment"]
             , source [t "bstore1.example.com"]
             , price [t "65.95"]
             ]
      , book [ title [t "TCP/IP Illustrated"]
             , source [t "bstore2.example.com"]
             , price     [t "65.95"]
             ]
      , book [ title [t "TCP/IP Illustrated"] 
             , source [t "bstore1.example.com"]
             , price     [t "65.95"]
             ]
      , book [ title  [t "Data on the Web"]
             , source [t "bstore2.example.com"]
             , price  [t "34.95"]
             ]
      , book [ title  [t "Data on the Web"] 
             , source [t "bstore1.example.com"]
             , price  [t "39.95"]
             ]
      ]
    where 
      t x       = N (T x) []
      prices    = N (E "prices") 
      price     = N (E "price") 
      book      = N (E "book") 
      title     = N (E "title")
      source    = N (E "source")


-- Q10
q10 t = pick $ do { ps <- gather $ 
                          do { ti  <- distinctTitles t
                             ; ps  <- gather $ 
                                      do { b   <- deep (ofLabel (el "book")) t
                                         ; ti' <- children b >>= ofLabel (el "title") >>= children 
                                         ; guardM $ lift $ liftEq (==) ti ti' 
                                         ; childrenOfLabel (el "price") b >>= children }
                             ; ps' <- mapM (lift . liftO1 id . label) ps 
                             ; return $ N (el "minprice") 
                                          [N (attr "title") [ti], 
                                           N (el "price") [N (new $ minim ps') []]] }
                  ; return $ N (el "results") ps }
    where
      minim = minimumBy (compare `on` f)
          where
            f (T x) = read x :: Float 
      distinctTitles t = 
          do { ts <- gather $ do { b <- deep (ofLabel (el "book")) t
                                 ; childrenOfLabel (el "title") b >>= children }
             ; ts' <- lift $ nubByM (liftEq (==)) ts 
             ; ListT $ return ts' }

              
-- Q11 
q11 t = pick $ do { bs1 <- gather $ booksWithAuthor t 
                  ; bs2 <- gather $ booksWithEditor t 
                  ; return $ N (el "bib") (bs1 ++ bs2) }
    where
      booksWithAuthor t = 
          do { b <- deep (ofLabel (el "book")) t 
             ; a <- children b >>= ofLabel (el "author")
             ; t <- children b >>= ofLabel (el "title")
             ; return $ N (el "book") [t,a] }
      booksWithEditor t =
          do { b <- deep (ofLabel (el "book")) t 
             ; e <- children b >>= ofLabel (el "editor")
             ; a <- children e >>= ofLabel (el "affiliation")
             ; t <- children b >>= ofLabel (el "title")
             ; return $ N (el "reference") [t,a]}
