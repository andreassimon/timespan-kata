{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Test.Framework
import Test.Framework.TestManager

prettyPrint :: Int -> String
prettyPrint seconds = _prettyPrint seconds 0

_prettyPrint :: Int -> Int -> String
_prettyPrint seconds 0
  | seconds < 60   =   (show seconds) ++ " seconds"
  | otherwise      =   _prettyPrint (seconds - 60) (1)
_prettyPrint seconds minutes
  | seconds < 60   =   (show minutes) ++ " minutes, " ++ (show seconds) ++ " seconds"
  | otherwise      =   _prettyPrint (seconds - 60) (minutes + 1)

test_1_1 =
    do assertEqual "30 seconds" (prettyPrint 30)
       assertEqual "20 seconds" (prettyPrint 20)

test_1_2 =
    do assertEqual "2 minutes, 20 seconds" (prettyPrint 140)
       assertEqual "3 minutes, 0 seconds" (prettyPrint 180)

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
