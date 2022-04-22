module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except

import Definitions
import Parser.NomadParser ( parse )
import Typesystem.TypeCheck

parse' :: String -> Either NomadError Stmt
parse' s = runExcept $ parse s

main :: IO ()
main = do
    defaultMain (testGroup "Parsing Tests" [parseTest])


parseTest :: TestTree
parseTest = testGroup "Testing parse"
    [testCase (assertEqual "" (Right (Expr (Num 1.0))) (parse' "1"))]


-- sayYoTest :: TestTree
-- sayYoTest = testCase "Testing sayYo"
--   (assertEqual "Should say Yo to Friend!" "Yo Friend!" (sayYo "Friend"))

-- add5Test :: TestTree
-- add5Test = testCase "Testing add5"
--   (assertEqual "Should add 5 to get 10" 10 (add5 5))
