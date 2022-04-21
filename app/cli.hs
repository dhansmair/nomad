{-
The main command line interface for the application.

Defines the main and loop functions, which handle the main transformer stack. 
Helper functions facilitate the handling of user inputs, execution of actions and output of the results.

-}
import System.IO
import System.Console.Haskeline
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.List
import Data.Char (isSpace)

import Interpreter ( evaluate, alphaRename )
import Parser.NomadParser ( parse )
import Builtins.Builtins (stdEnv)
import Typesystem.TypeCheck ( getType, getTypeEither )
import Definitions
import Environment

type Command = String -> NomadExceptT (EnvT IO) ()

commands :: [(String, Command, Bool)]
commands = [ ("t", showType, True)
           , ("env", printEnv, False)
           ]


-- Evaluates statements by evaluating expressions and managing definitions
performAction :: Stmt -> NomadExceptT (EnvT IO) ()
performAction (Expr ex) = do
    t <- getType (alphaRename ex)
    d <- evaluate ex
    liftIO $ putStrLn $ "  " ++ showEx d ++ "\t:: " ++ show t
performAction (Def s ex) = lift $ do
    t <- getTypeEither (alphaRename ex)
    addDefnT (s, ex, t)
    reevalDepsT s
    case t of
      Right t' -> lift $ putStrLn $ "  " ++ s ++ " = " ++ showEx ex ++ "\t:: " ++ show t'
      Left err -> lift $ putStrLn $ "  " ++ s ++ " = " ++ showEx ex ++ "\t:: [" ++ show err ++ "]"

-- :t command. Prints the type of an expression
showType :: Command
showType line = do
    stmt <- parse line
    case stmt of
        (Expr ex) -> do
            t <- getType (alphaRename ex)
            liftIO $ putStr "type: "
            liftIO $ print t
        _ -> throwError $ BlankError $ "\"" ++ line ++ "\" is not an expression"

-- :env command. Prints the Environment.
printEnv :: Command
printEnv _ = lift $ do
    env <- getEnvT
    lift $ printLines env
    where 
        printLines (x:xs) = do 
            print x
            printLines xs
        printLines [] = return ()


isEmptyString :: String -> Bool
isEmptyString "" = True
isEmptyString s = all isSpace s


handleInput :: String -> NomadExceptT (EnvT IO) () 

-- handle commands, starting with ':'
handleInput (':':line) = do
    let result = find (\(s, _, _) -> s `isPrefixOf` line) commands
    case result of
        Nothing -> do
            throwError $ InvalidCommandError $ "command \":" ++ line ++ "\" does not exist"
        Just (prefix, command, hasArgs) -> do
            let line' = dropWhile isSpace $ drop (length prefix) line

            if hasArgs && isEmptyString line' 
                then throwError $ InvalidCommandError $ "command \":" ++ prefix ++ "\" requires an argument"
            else if not hasArgs && not (isEmptyString line') 
                then throwError $ InvalidCommandError $ "command \":" ++ prefix ++ "\" requires no arguments"
            else
                command line'
-- if input does not start with :, try to parse it directly
handleInput line = do
    stmt <- parse line
    performAction stmt

-- Main loop of the cli app. Receives and delegates user input and restarts itself.
loop :: InputT (EnvT IO) ()
loop = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
            let input' = dropWhile isSpace input
            r <- lift $ runExceptT $ handleInput input'
            case r of 
                Left err -> do
                    liftIO $ print err
                    loop
                Right _ -> loop

-- Initializes main loop and prints a welcome message.
main :: IO ()
main = do
    putStrLn welcomeText
    runEnvT (runInputT defaultSettings loop) stdEnv
    return ()
  where 
    welcomeText = "Hello! This is the Nomad Calculator! (type :q to quit) \n\
    \-----------------------------------------------------------------"
