module Typesystem.Unification3 ( mostGeneralUnifier ) where


import Control.Monad ( (>=>) )
import Data.List ( find )
import Data.Maybe ( fromMaybe )

import Definitions


mostGeneralUnifier :: Type -> [TypeEquation] -> Maybe Type
mostGeneralUnifier t eqs = do
    eqs' <- unify eqs
    return $ replaceVariables t eqs'
    where
        replaceVariables :: Type -> [TypeEquation] -> Type
        replaceVariables TNum _  = TNum
        replaceVariables (TVar x) eqs = fromMaybe (TVar x) (lookup (TVar x) eqs)
        replaceVariables (TArr l r) eqs = 
            let l' = replaceVariables l eqs
                r' = replaceVariables r eqs
             in TArr l' r'

unify :: [TypeEquation] -> Maybe [TypeEquation]
unify eqs = do 
    eqs' <- composeM [elim, failCheck, occursCheck, orient, decompose, solve] eqs 
    if eqs == eqs' then return eqs else unify eqs'
    where 
        composeM [] = return
        composeM (f:fs) = foldl (>=>) f fs 


failCheck :: [TypeEquation] -> Maybe [TypeEquation]
failCheck = mapM failCheck'
    where
        failCheck' (TArr _ _, TNum) = Nothing
        failCheck' (TNum, TArr _ _) = Nothing
        failCheck' other = Just other

occursCheck :: [TypeEquation] -> Maybe [TypeEquation]
occursCheck = mapM occursCheck'
    where
        occursCheck' eq@(TVar x, TArr l r) = do
            if appearsIn x (TArr l r) then Nothing
            else Just eq
        occursCheck' eq = Just eq

elim :: [TypeEquation] -> Maybe [TypeEquation]
elim eqs = Just $ filter (uncurry (/=)) eqs

orient :: [TypeEquation] -> Maybe [TypeEquation]
orient eqs = Just $ map orientEq eqs
    where
        orientEq (TNum, TVar x) = (TVar x, TNum)
        orientEq (TArr l r, TVar x) = (TVar x, TArr l r)
        orientEq other = other

decompose :: [TypeEquation] -> Maybe [TypeEquation]
decompose eqs = Just $ concatMap decompEq eqs
    where
        decompEq (TArr l1 r1, TArr l2 r2) = [(l1, l2), (r1, r2)]
        decompEq other = [other]

-- TODO refactor
solve :: [TypeEquation] -> Maybe [TypeEquation]
solve eqs = case findSolver eqs of
                Nothing -> Just eqs
                Just solver -> Just $ solveWith eqs solver
    where
    -- check if there is an equation of pattern (var, _), where var either appears
    -- twice on the left side, or at least once on the right side in the list of all type equations.
    -- returns a type equation that satisfies that condition
    findSolver :: [TypeEquation] -> Maybe TypeEquation
    findSolver list = find (\(lhs, rhs) -> canSolve lhs list) list

    canSolve :: Type -> [TypeEquation] -> Bool
    canSolve (TVar s) eqs = countLeftSide s eqs >= 2 || any (appearsIn s . snd) eqs
    canSolve _ _ = False

    countLeftSide :: String -> [TypeEquation] -> Int
    countLeftSide s eqs = length $ filter (appearsIn s) (map fst eqs)

    solveWith :: [TypeEquation] -> TypeEquation -> [TypeEquation]
    solveWith list eq = eq : map (replaceWithEq eq) list

    replaceWithEq :: TypeEquation -> TypeEquation -> TypeEquation 
    replaceWithEq eq (lhs, rhs) = (replaceWith eq lhs, replaceWith eq rhs)

    replaceWith :: TypeEquation -> Type -> Type
    replaceWith (a,b) (TArr l r)   
        | a == TArr l r = b
        | otherwise = TArr (replaceWith (a, b) l) (replaceWith (a, b) r)
    replaceWith (a,b) structure    | a == structure = b
                                   | otherwise = structure

appearsIn :: String -> Type -> Bool
appearsIn s TNum       = False
appearsIn s (TVar x)   = s == x
appearsIn s (TArr l r) = appearsIn s l || appearsIn s r


-- an attempt to refactoring
-- solve' :: [TypeEquation] -> Maybe [TypeEquation]
-- solve' eqs = do 
--     Nothing
--     where
--         helper :: TypeEquation -> [TypeEquation] -> [TypeEquation]
--         helper eq list = do 
--             let list' = map (replaceWithEq eq) list
--             let list'' = filter (uncurry (/=)) list'

--         replaceWithEq :: TypeEquation -> TypeEquation -> TypeEquation 
--         replaceWithEq eq (lhs, rhs) = (replaceWith eq lhs, replaceWith eq rhs)

--         replaceWith :: TypeEquation -> Type -> Type
--         replaceWith eq@(a,b) typ@(TArr l r)   
--             | a == typ  = b
--             | otherwise = TArr (replaceWith eq l) (replaceWith eq r)
--         replaceWith (a,b) structure    | a == structure = b
--                                        | otherwise = structure

