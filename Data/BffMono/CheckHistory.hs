{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Data.BffMono.CheckHistory where 


-- | 'CheckResult' stores an observation result.  It consists of an
--   observation function, a list observed elements and an observation
--   result. We used an existential type here to store heterogeneous
--   observation results into a list.

data CheckResult a = forall b. Eq b => 
                     CheckResult ([a] -> b) [a] b 
                     
-- | Checks if an update does not change a recorded observation
checkResult :: Monad m => (a -> m a) -> CheckResult a -> m Bool 
checkResult u (CheckResult test as r) = 
    do { as' <- mapM u as 
       ; return $ test as' == r }

-- | Checks if an update does not change all the recorded observation results 
checkHistory :: Monad m => (a -> m a) -> [CheckResult a] -> m Bool 
checkHistory u hist = 
    do { bs <- mapM (checkResult u) hist 
       ; return $ and bs }

