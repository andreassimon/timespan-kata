{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Test.Framework
import Test.Framework.TestManager

test_nonEmpty = 
    do assertEqual [1] [1]

test_empty = assertEqual ([] :: [Int]) ([] :: [Int])

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
