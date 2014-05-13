{-# LANGUAGE RankNTypes, FunctionalDependencies, FlexibleContexts, 
             FlexibleInstances, ExistentialQuantification, 
             DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.List ((\\), nub)
import Data.Traversable (Traversable, traverse) 
import Control.Applicative (getConst, Const(..))

import Data.Foldable (Foldable) 

import Control.Monad.State (evalState) 
import qualified Control.Monad.State

import qualified Data.BffMono.EquivWitness as EW 
import Control.Monad (liftM, mzero, MonadPlus, (>=>)) 

import Control.Monad.List  (ListT(..))
import Control.Monad.Trans (lift) 

{-
---------
Section 2 
---------
-}

data LocOrg a = LocOrg { bodyOrg :: a, locationOrg :: Int}

type Update a = [(Int,a)]

updateOrg :: [(Int,a)] -> LocOrg a -> LocOrg a 
updateOrg upd (LocOrg x i) = 
    maybe (LocOrg x i) (\y -> LocOrg y i) (lookup i upd) 

matchViewsSimple :: Eq a => [LocOrg a] -> [a] -> Update a 
matchViewsSimple vx v = 
    if length vx == length v then 
        minimizeOrg vx $ makeUpdSimple $ zip vx v 
    else
        error "Shape Mismatch" 

makeUpdSimple :: Eq a => [(LocOrg a,a)] -> Update a 
makeUpdSimple = foldr f []
    where
      f (LocOrg _ i,y) u =
          case lookup i u of 
            Nothing -> 
                (i,y):u
            Just y' | y'==y     -> u
                    | otherwise -> error "Inconsistent Update"

minimizeOrg :: Eq a => [LocOrg a] -> Update a -> Update a 
minimizeOrg vx u = u \\ [(i,x) | LocOrg x i <- vx] 


bwdOrg :: (forall a. [a] -> [a]) -> (forall c. Eq c => [c] -> [c] -> [c])
bwdOrg h = \ s v -> 
           let sx  = zipWith LocOrg s [1..]
               vx  = h sx 
               upd = matchViewsSimple vx v
           in map (bodyOrg . updateOrg upd) sx

{-
---------
Section 3 
---------
-}
data Tree a = Node a [Tree a]
            deriving (Show, Eq, Functor, Foldable, Traversable) 

srcLinks = Node "root" [Node "a" [Node "text" []],
                        Node "p" [Node "a" [Node "text2" []]]]


viewLinks = Node "results" [Node "a" [Node "text" []], 
                            Node "a" [Node "text2" []]]

--- Section 3.1 

linksMono :: Tree String -> Tree String 
linksMono t = Node "results" (linkssMono t)

linkssMono :: Tree String -> [Tree String]
linkssMono (Node n ts) =
    if n == "a" then [Node n ts]
    else             concatMap linkssMono ts 

class PackTrial c a | a -> c where
    new' :: c -> a 
    eq'  :: Eq c => a -> a -> Bool 


linksPoly :: forall a . PackTrial String a => Tree a -> Tree a
linksPoly t = Node (new' "results") (linkssPoly t)

linkssPoly :: forall a . PackTrial String a => Tree a -> [Tree a]
linkssPoly (Node n ts) =
    if eq' n (new' "a") then [Node n ts]
    else                     concatMap linkssPoly ts 

-- Section 3.2 

class (Pack c a, Monad m) => PackM c a m where
    liftO :: Eq b => ([c] -> b) -> ([a] -> m b)
    eqSync :: Eq c => a -> a -> m Bool -- Section 5.2 
             
class Pack c a | a -> c where
    new :: c -> a 

liftO2 p x y = liftO (\[x,y] -> p x y) [x,y]

-- Section 3.3 

links :: forall a. forall m. PackM String a m => Tree a -> m (Tree a)
links t = do as <- linkss t
             return $ Node (new "results") as

linkss :: forall a. forall m. PackM String a m => Tree a -> m [Tree a]
linkss (Node n ts) =
    do b <- liftO2 (==) n (new "a")
       if b then return [Node n ts]
       else      concatMapM linkss ts 
    where concatMapM h x = do ys <- mapM h x 
                              return $ concat ys

newtype I a = I {runI :: a}
newtype N a = N {runN :: a }

instance Functor I where 
    fmap f = I . f . runI 

instance Monad I where 
    return = I
    I x >>= f = f x 

instance Functor N where 
    fmap f = N . f . runN

instance Pack c (N c) where 
    new = N

instance PackM c (N c) I where
    liftO p x = I (p $ map runN x) 
    eqSync = liftO2 (==) -- Section 5.2 

fwdTree :: (forall a. forall m. PackM c a m => Tree a -> m (Tree a))
           -> Tree c -> Tree c 
fwdTree h = \s -> let I v = h (fmap N s) in fmap runN v 

linksF :: Tree String -> Tree String 
linksF = fwdTree links

-- Section 3.4 

data Loc a = Loc { body :: a, location :: Maybe Int } 

instance Show a => Show (Loc a) where 
    show (Loc a (Just i)) = "(" ++ show a ++ "@" ++ show i ++ ")"
    show (Loc a Nothing ) = "(" ++ show a ++ "@#" ++ ")" 

s @@ i  = Loc s (Just i)
s @# _  = Loc s Nothing 

srcxLinks = Node ("root" @@ 1) [
             Node ("a" @@ 2) [Node ("text" @@ 3) []],
             Node ("p" @@ 4) [Node ("a" @@ 5) [Node ("text2" @@ 6) []]]]

viewxLinks = Node ("results" @# ()) [
              Node ("a" @@ 2) [Node ("text" @@ 3) []],
              Node ("a" @@ 5) [Node ("text2" @@ 6) []]]


instance Pack c (Loc c) where 
    new x = Loc x Nothing


data Result a = forall b. Eq b => Result ([a] -> b) [a] b 


check :: Result a -> Bool 
check (Result p xs r) = p xs == r 

checkHist :: (a -> a) -> [Result a] -> Bool 
checkHist u h = all (check . u') h 
    where u' (Result p xs r) = Result p (map u xs) r 

newtype W a b = W (b,[Result a]) 

instance Monad (W a) where 
    return x = W (x, []) 
    W (x,h1) >>= f = 
        let W (y,h2) = f x 
        in W (y, h1++h2) 

instance PackM c (Loc c) (W (Loc c)) where 
    liftO p x = W (p' x, [Result p' x (p' x)]) 
        where p' = p . map body 

    eqSync = liftO2 (==) -- We don't provide eqSync for this instance. 

update :: Update c -> Loc c -> Loc c
update upd (Loc x Nothing) = Loc x Nothing 
update upd (Loc x (Just i)) =
    maybe (Loc x (Just i)) (\y -> Loc y (Just i)) (lookup i upd) 
       
bwdTree :: Eq c => 
       (forall a. forall m. PackM c a m => Tree a -> m (Tree a)) 
           -> Tree c -> Tree c -> Tree c 
bwdTree h = 
    \s v -> 
        let sx = assignLocs  s
            W (vx, hist) = h sx 
            upd          = matchViews vx v
        in if checkHist (update upd) hist then 
               fmap (body . update upd) sx 
           else 
               error "Inconsistent History" 


linksB :: Tree String -> Tree String -> Tree String
linksB = bwdTree links 

view' = Node "results" [Node "a" [Node "chagned" []], 
                        Node "a" [Node "text2" []]]

{- 
*Main> linksB srcLinks view'
Node "root" [Node "a" [Node "chagned" []],Node "p" [Node "a" [Node "text2" []]]]
-}

view'' = Node "results" [Node "b" [Node "text" []], 
                         Node "a" [Node "text2" []]]

{-
*Main> linksB srcLinks view''
*** Exception: Inconsistent History
-}


{-
---------
Section 4
---------
-} 




{-
---------
Section 5
---------
-}

-- Section 5.1 
contents :: Traversable k => k a -> [a]
contents = getConst . traverse (\x -> Const [x])

fill :: Traversable k => k b -> [a] -> k a 
fill t l = evalState (traverse next t) l 
    where next _ = do (a:x) <- Control.Monad.State.get
                      Control.Monad.State.put x 
                      return a 

assignLocs :: Traversable k => k c -> k (Loc c) 
assignLocs t = fill t (assignLocsList $ contents t) 
    where assignLocsList l = zipWith (\x i -> Loc x (Just i)) l [1..]

matchViews :: (Traversable k, Eq (k ()), Eq c) => 
              k (Loc c) -> k c -> Update c 
matchViews vx v = if fmap ignore vx == fmap ignore v then 
                      let lx = contents vx 
                          l  = contents v
                      in minimize lx $ makeUpd $ zip lx l 
                  else
                      error "Shape Mismatch"
    where
      ignore _ = () 
                            
makeUpd :: Eq c => [(Loc c, c)] -> Update c 
makeUpd = foldr f []
    where
      f (Loc x Nothing, y) u | x == y    = u 
                             | otherwise = error "Update of Constants" 
      f (Loc _ (Just i), y) u =
          case lookup i u of 
            Nothing -> (i,y):u 
            Just y' | y==y'     -> u
                    | otherwise -> error "Inconsistent Update" 
                        
-- NB: This function is currently not included in the paper. 
minimize :: Eq c => [Loc c] -> Update c -> Update c
minimize vx u = u \\ [(i,x) | Loc x (Just i) <- vx] 
                             

fwd :: (Traversable k1, Traversable k2) => 
       (forall a. forall m. PackM c a m => k1 a -> m (k2 a))
       -> k1 c -> k2 c 
fwd h = \s -> let I v = h (fmap N s) in fmap runN v 
            

bwd :: (Traversable k1, Traversable k2, Eq (k2 ()), Eq c) => 
       (forall a. forall m. PackM c a m => k1 a -> m (k2 a))
       -> k1 c -> k2 c -> k1 c 
bwd h = \s v -> 
        let sx = assignLocs  s
            W (vx, hist) = h sx 
            upd          = matchViews vx v
        in if checkHist (update upd) hist then 
               fmap (body . update upd) sx 
           else 
               error "Inconsistent History" 

-- Section 5.2 

type Equiv = EW.EquivWitness Int 

newtype SW a b = SW { runSW :: Equiv -> ((b,[Result a]), Equiv) }

instance Monad (SW a) where 
    return x = SW $ \e -> ((x,[]), e)
    SW g >>= f = SW $ \e -> let ((x,h1),e') = g e 
                                ((y,h2),e'') = runSW (f x) e' 
                            in ((y,h1++h2), e'')
                 
empty :: Equiv 
empty = EW.empty 

equate :: Int -> Int -> Equiv -> Equiv 
equate = EW.equate 

equal :: Int -> Int -> Equiv -> Bool 
equal i j e = fst $ EW.equals i j e 
        
    
instance PackM c (Loc c) (SW (Loc c)) where
    liftO p x = SW $ \ e -> ((p' x, [Result p' x (p' x)]),e)
        where p' = p . map body 
    eqSync x y | body x == body y, Just i <- location x, Just j <- location y =
                        SW $ \e -> let (z,e') = runSW (liftO2 (==) x y) e 
                                   in (z, equate i j e')
               | otherwise = liftO2 (==) x y 


bwdE :: (Traversable k1, Traversable k2, Eq (k2 ()), Eq c) => 
        (forall a. forall m. PackM c a m => k1 a -> m (k2 a))
        -> k1 c -> k2 c -> k1 c 
bwdE h = \s v -> 
        let sx                  = assignLocs  s
            ((vx, hist), equiv) = runSW (h sx) empty 
            upd                 = matchViewsE equiv vx v
        in if checkHist (updateE equiv upd) hist then 
               fmap (body . updateE equiv upd) sx 
           else 
               error "Inconsistent History" 


updateE :: Equiv -> Update c -> Loc c -> Loc c 
updateE equiv upd (Loc x Nothing) = Loc x Nothing 
updateE equiv upd (Loc x (Just i)) =
    maybe (Loc x (Just i)) (\y -> Loc y (Just i)) (lookupBy (\i j -> equal i j equiv) i upd) 


matchViewsE :: (Traversable k, Eq (k ()), Eq c) => 
               Equiv -> k (Loc c) -> k c -> Update c 
matchViewsE equiv vx v = 
    if fmap ignore vx == fmap ignore v then 
        let lx = contents vx 
            l  = contents v
        in minimize lx $ makeUpdE equiv $ zip lx l 
    else
        error "Shape Mismatch"
    where
      ignore _ = () 
                            
makeUpdE :: Eq c => Equiv -> [(Loc c, c)] -> Update c 
makeUpdE equiv = foldr f []
    where
      f (Loc x Nothing, y) u | x == y    = u 
                             | otherwise = error "Update of Constants" 
      f (Loc _ (Just i), y) u =
          case lookupBy (\i j -> equal i j equiv) i u of 
            Nothing -> (i,y):u 
            Just y' | y==y'     -> u
                    | otherwise -> error "Inconsistent Update" 
                                          
                                             
lookupBy eq x [] = Nothing 
lookupBy eq x ((k,v):ys) | eq x k    = Just v
                         | otherwise = lookupBy eq x ys 

nubM :: (Eq c, PackM c a m) => [a] -> m [a]
nubM [] = return []
nubM (x:xs) = do r <- deleteAllM x xs 
                 xs' <- nubM r 
                 return (x:xs')

deleteAllM :: (Eq c, PackM c a m) => a -> [a] -> m [a]
deleteAllM x [] = return []
deleteAllM x (y:ys) = do b <- eqSync x y 
                         r <- deleteAllM x ys 
                         return (if b then r else y:r)

{- 
---------
Section 6
--------- 
-} 

-- Section 6.1

countWords :: [String] -> [(String,Int)]
countWords [] = []
countWords (w:ws) = let (c,ws') = deleteAndCount w ws 
                    in (w, c+1) : countWords ws' 

deleteAndCount :: String -> [String] -> (Int,[String])
deleteAndCount x [] = (0,[])
deleteAndCount x (w:ws) = let (c,ws') = deleteAndCount x ws
                          in if x == w then (c+1,ws') else (c, w:ws')


countWordsM :: PackM String a m => [a] -> m [(a,Int)]
countWordsM []     = return []
countWordsM (w:ws) = do (c,ws') <- deleteAndCountM w ws 
                        r <- countWordsM ws'
                        return $ (w,c+1):r 

deleteAndCountM :: PackM String a m => a -> [a] -> m (Int, [a])
deleteAndCountM x [] = return (0,[])
deleteAndCountM x (w:ws) = 
    do (c,ws') <- deleteAndCountM x ws
       b <- eqSync x w 
       return (if b then (c+1,ws') else (c,w:ws'))

newtype CountList a = CountList { runCountList :: [(a,Int)] }
    deriving (Functor, Foldable, Traversable, Eq) 

countWordsF :: [String] -> [(String,Int)]
countWordsF ws = runCountList $ fwd (liftM CountList . countWordsM) ws 

countWordsB :: [String] -> [(String,Int)] -> [String]
countWordsB ws cs = bwdE (liftM CountList . countWordsM) ws (CountList cs)

listFigure4 = 
    words $
    unlines ["Old MacDonald had a farm E-I-E-I-O"
            , "And on that farm he had a cow E-I-E-I-O"
            , "With a moo moo here and a moo moo there"
            , "Here a moo" ++" "++ "there a moo" ++" "++  "everywhere a moo moo"
            , "Old MacDonald had a farm E-I-E-I-O"]


-- Section 6.2

data L = A String | E String | T String deriving (Eq, Show) 

testXML =
    Node (E "book") [Node (A "year")  [Node (T "1994") []],
                     Node (E "title") [Node (T "Text") []]]

label (Node l _) = l 

keep :: PackM L a m => Tree a -> ListT m (Tree a)
keep = return 

children :: PackM L a m => Tree a -> ListT m (Tree a)
children (Node _ ts) = ListT (return ts) 

ofLabel :: PackM L a m => a -> Tree a -> ListT m (Tree a)
ofLabel l t = do guardM $ lift $ liftO2 (==) (label t) l 
                 return t 

guardM :: MonadPlus k => k Bool -> k ()
guardM x = x >>= (\b -> if b then return () else mzero) 

q1 :: PackM L a m => Tree a -> m (Tree a)
q1 t = pick $ do bs <- gather $ (keep /> (tag "book" >=> h)) t
                 return $ Node (new $ E "bib") bs 
    where
      h b = do y <- (keep /> attr "year" /> keep) b 
               t <- (keep /> tag "title") b 
               p <- (keep /> tag "publisher" /> keep) b 
               guardM $ lift $ liftO2 gtInt (label y) (new $ T "1991")
               guardM $ lift $ liftO2 (==)  (label p) (new $ T "Addison-Wesley")
               return $ Node (new $ E "book") [Node (new $ A "year") [y], t]
      gtInt (T l1) (T l2) = (read l1 :: Int) > (read l2 :: Int) 

tag  s = ofLabel (new $ E s)
attr s = ofLabel (new $ A s)

gather :: Monad m => ListT m a -> ListT m [a]
gather (ListT m) = ListT $ do { x <- m; return [x] }

pick :: Monad m => ListT m a -> m a 
pick (ListT x) = do { a <- x; return $ head a }


(/>) :: PackM L a m => 
        (Tree a -> ListT m (Tree a))
        -> (Tree a -> ListT m (Tree a))
        -> (Tree a -> ListT m (Tree a))
f /> g = f >=> children >=> g 

-- Section 6.3

newtype Graph a = Graph [(Int,a,Int)]
    deriving (Functor, Foldable, Traversable, Eq, Show) 

srcGraph = 
    Graph [ (0,"members",1), (1,"mem",2), (1,"mem",8)
          , (2,"name",3), (2,"contact",5), (2,"friend",8)
          , (3,"Matsuda",4), (5,"phone",6), (6,"+81-90-XXXX-YYYY",7)
          , (8,"friend",2), (8,"name",9), (9,"Wang",10)
          , (8,"contact",11), (11,"mail",12),(12,"wmeng@...",13) ]


m2m :: PackM String a m => Graph a -> m (Graph a)
m2m = rename "mem" "member" 
      >=> rename "friend" "knows"
      >=> contract "contact" 

rename :: PackM String a m => String -> String -> Graph a -> m (Graph a)
rename x y (Graph es) = 
    do r <- mapM (\(s,e,d) -> 
                      do b <- liftO2 (==) e (new x)
                         return (if b then (s,new y,d) else (s,e,d))) es 
       return $ Graph r 


contract :: PackM String a m => String -> Graph a -> m (Graph a) 
contract x (Graph es) = 
    do let nodes = nub (concatMap (\ (s,_,d)  -> [s,d]) es) :: [Int]
       conts <- concatMapM (\(s,e,d) -> 
                              do b <- liftO2 (==) e (new x)
                                 return (if b then [(s,d)] else [])) es 
       let rstCls = mkSymTransClosure (conts ++ [(z,z) | z <- nodes]) :: [(Int,Int)]

       let repr z = minimum [ y | (z',y) <- rstCls, z' == z ]
       es' <- concatMapM  (\(s,e,d) -> 
                               do b <- liftO2 (==) e (new x)
                                  return (if b then [] else [(repr s, e, repr d)])) es
       ; return $ Graph es' 
    where concatMapM h x = do ys <- mapM h x 
                              return $ concat ys
          mkSymTransClosure :: [(Int,Int)] -> [(Int,Int)]
          mkSymTransClosure rs = mkTransClosure $ nub $ rs ++ [ (y,x) | (x,y) <- rs, x /= y ]
          mkTransClosure :: [(Int,Int)] -> [(Int,Int)]
          mkTransClosure rs = 
              let rs' = nub $ rs ++ [ (x,z) | (x,y) <- rs, (y',z) <- rs, y == y' ]
              in if rs == rs' then rs else mkTransClosure rs'

viewGraph = 
    Graph [ (0,"members",1), (1,"member",2), (1,"member",8)
          , (2,"name",3), (2,"knows",8)
          , (3,"Matsuda",4), (2,"phone",6), (6,"+81-90-XXXX-YYYY",7)
          , (8,"knows",2), (8,"name",9), (9,"Wang",10)
          , (8,"mail",12),(12,"wmeng@...",13) ]

updGraph1 = 
    Graph [ (0,"members",1), (1,"member",2), (1,"member",8)
          , (2,"name",3), (2,"knows",8)
          , (3,"Matsuda",4), (2,"mail",6), (6,"kztk@...",7)
          , (8,"knows",2), (8,"name",9), (9,"Wang",10)
          , (8,"mail",12),(12,"wmeng@...",13) ]

{-
--------- 
Section 7
---------
-}

ff [x,y] = liftO2 (==) x y >> return [x] 

hasChanged (Loc x _, y) = not (x == y) 

dup [x] = return [x,x]

bwdE' :: (Traversable k1, Traversable k2, Eq (k2 ()), Eq c) => 
         (forall a. forall m. PackM c a m => k1 a -> m (k2 a))
         -> k1 c -> k2 c -> k1 c 
bwdE' h = \s v -> 
        let sx                  = assignLocs  s
            ((vx, hist), equiv) = runSW (h sx) empty 
            upd                 = matchViewsE' equiv vx v
        in if checkHist (updateE equiv upd) hist then 
               fmap (body . updateE equiv upd) sx 
           else 
               error "Inconsistent History" 


matchViewsE' :: (Traversable k, Eq (k ()), Eq c) => 
                Equiv -> k (Loc c) -> k c -> Update c 
matchViewsE' equiv vx v = 
    if fmap ignore vx == fmap ignore v then 
        let lx = contents vx 
            l  = contents v
        in makeUpdE equiv $ filter hasChanged $ zip lx l 
    else
        error "Shape Mismatch"
    where
      ignore _ = () 

dupB [x] = bwdE' dup [x]
