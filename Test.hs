{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Language.CheapB18n 
import Data.Functor
import Data.Foldable hiding (concat)
import Data.Traversable hiding (mapM)

import Control.Applicative ((<$>))

-- | Tree with node type a 
data Tree a = Node a [Tree a]
            deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- | An atomic datatype we want to abstract.
--   We assume that the only observation is to 
--   check its equality. 
-- 
--   The type |Label| abstracts the XML elements and PCData. 
data Label = LN String | LText String
           deriving (Eq,Ord,Show)

ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM x t f = 
    x >>= (\b -> if b then t else f)

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
