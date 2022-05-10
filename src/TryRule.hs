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

