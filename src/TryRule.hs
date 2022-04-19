module TryRule where

import Definitions


type GuessFunc a = a -> Either NomadError (Maybe a)

applyUntilConvergence :: [GuessFunc a] -> a -> Either NomadError a 
applyUntilConvergence list val = do
    val' <- findFirst val list
    case val' of
        Nothing -> return val
        Just sth -> applyUntilConvergence list sth

findFirst :: a -> [GuessFunc a] -> Either NomadError (Maybe a) 
findFirst val [] = Right Nothing
findFirst val (f:fs) = 
    case f val of
        Right Nothing -> findFirst val fs
        other -> other

checkAll :: (a -> Bool) -> NomadError -> GuessFunc [a] 
checkAll f err eqs =  
    if any f eqs 
    then Left err 
    else Right Nothing




------------------------------------------------------
-- use case for simplification
------------------------------------------------------
-- TODO this needs yet to be elaborated

------------------------------------------------------
-- use case for unification
------------------------------------------------------
-- data RuleStatus = Unchanged | Changed 
--     deriving(Show, Eq)

-- type TryRuleT m a b = StateT (RuleStatus, a) m b

-- tryApply :: Monad m => ... -> TryRuleT m a a
-- tryApply func = do
--     (s, val) <- get
--     case s of 
--         Changed -> do
--             let val' = func val
--             put (s, val')
--             return val' 
--         Unchanged -> return val

-- ifChanged :: Monad m => (a -> a) -> TryRuleT m a a
-- ifChanged func = do
--     (s, val) <- get
--     case s of 
--         Changed -> do
--             let val' = func val
--             put (s, val')
--             return val' 
--         Unchanged -> return val


-- data TryRule a b = Changed a | Unchanged b
--     deriving(Show)

-- instance Functor (TryRule a b) where
--     fmap f (Unchanged a) = Changed (f a)
--     fmap f (Changed a) = Changed a

-- instance Applicative TryRule where
--     pure = return
--     -- (TryRule s1 a1 f) <*> (TryRule s2 a2 x) = TryRule Changed (f x) (f x)     
--     -- (TryRule s1 a1 f) <*> trx = fmap f trx
--     (Changed f) <*> trx = fmap f trx
--     (Unchanged f) <*> trx = fmap f trx

-- instance Monad TryRule where
--     return x = Changed x
--     (Unchanged a) >>= f = do
--         f a
--     Changed a >>= f = f a


-- funktionen, die auf die Monade angewendet werden,
-- sollen auf den Daten-Inhalt angewandt werden, wenn der status unchanged ist.
-- Die Funktion selbst soll dann außerdem mitteilen, ob der Inhalt geändert wurde oder nicht.
-- oder soll das die Monade tracken..?
-- Beispiel-Anwendung:
--
--  trySolve :: data -> TryRule
--  trySolve something = something
--  trySolve other = do
--      ...
--      modified = ...
--      ...
--      return modified
--
-- return soll in diesem Fall automatisch den Status auf 'Changed' setzen.
--

-- unify equations
--   (res1, b1) = try orient equations
--   (res2, b2) = try solve res1
--   (res3, b3) = try decompose res2
--
--  if any True [b1, b2, b3] then
--      -- recursion
--      unify res3
--  else
--      return res3
--
--
-- unify' equations
--      try orient equations
--      try solve equations
--      try decompose equations
--
--      if changed 
--      then
--          unify' equations
--      else
--          return equations
--
--
-- unify'' :: State (bool, equations)
-- unify'' = do
--      attempt orient
--      attempt solve
--      attempt decompose
--      (s, eqs) <- get
--
--      if s == changed
--      then
--          unify''
--      else
--          return eqs
--
-- attempt :: (a -> Maybe a) -> State (bool, a) bool
-- attempt func = do
--      (s, a) <- get
--      case func a of
--          Nothing -> return false
--          Just res -> do
--              put (True, res)
--              return True
--
--
-- applyUntilConvergence :: [(a -> Maybe a)] -> a -> a 
-- applyUntilConvergence funcs a = do
--      ...
--
-- unify = applyUntilConvergence [orient, solve, decompose] equations
--
--
-- Maybe (Either changed unchanged)
--
-- type GuessFunc a = a -> Either SomeError (Maybe a)
