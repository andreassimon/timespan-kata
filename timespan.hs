{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Test.Framework
import Test.Framework.TestManager

prettyPrint :: Int -> String
prettyPrint seconds = (show seconds) ++ " seconds"

test_1_1 =
    do assertEqual "30 seconds" (prettyPrint 30)
       assertEqual "20 seconds" (prettyPrint 20)

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
