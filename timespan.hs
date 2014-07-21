{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Test.Framework
import Test.Framework.TestManager

type Seconds = Integer
type Minutes = Integer
type Hours = Integer
type Days = Integer
type Months = Integer
type Years = Integer

type Timespan = (Seconds, Minutes, Hours, Days, Months, Years)

prettyPrint :: Integer -> String
prettyPrint seconds = _prettyPrint $ simplify (seconds, 0, 0, 0, 0, 0)

simplify :: Timespan -> Timespan
simplify (seconds, minutes, hours, days, months, years)
  | seconds >= 60  =  simplify (seconds `mod` 60, minutes+(seconds `div` 60), hours, days, months, years)
  | minutes >= 60  =  simplify (seconds, minutes `mod` 60, hours+(minutes `div` 60), days, months, years)
  | hours   >= 24  =  simplify (seconds, minutes, hours `mod` 24, days+(hours `div` 24), months, years)
  | days    >= 30  =  simplify (seconds, minutes, hours, days `mod` 30, months+(days `div` 30), years)
  | months  >= 12  =  simplify (seconds, minutes, hours, days, months `mod` 12, years+(months `div` 12))
  | otherwise      =  (seconds, minutes, hours, days, months, years)

_prettyPrint :: Timespan -> String
_prettyPrint (seconds,       0,     0,    0,      0,     0) = (show seconds) ++ " seconds"
_prettyPrint (seconds, minutes,     0,    0,      0,     0) = (show minutes) ++ " minutes, " ++ (_prettyPrint (seconds, 0, 0, 0, 0, 0))
_prettyPrint (seconds, minutes, hours,    0,      0,     0) = (show hours) ++ " hours, " ++ (_prettyPrint (seconds, minutes, 0, 0, 0, 0))
_prettyPrint (seconds, minutes, hours, days,      0,     0) = (show days) ++ " days, " ++ (_prettyPrint (seconds, minutes, hours, 0, 0, 0))
_prettyPrint (seconds, minutes, hours, days, months,     0) = (show months) ++ " months, " ++ (_prettyPrint (seconds, minutes, hours, days, 0, 0))
_prettyPrint (seconds, minutes, hours, days, months, years) = (show years) ++ " years, " ++ (_prettyPrint (seconds, minutes, hours, days, months, 0))

test_1_1 =
    do assertEqual "30 seconds" (prettyPrint 30)
       assertEqual "20 seconds" (prettyPrint 20)

test_1_2 =
    do assertEqual "2 minutes, 20 seconds" (prettyPrint 140)
       assertEqual "3 minutes, 0 seconds" (prettyPrint 180)

-- Implement support for hours (days, months and years (we assume 30 days = 1 month, 1 year = 12 months))
test_1_3_hours =
    do assertEqual "2 hours, 2 minutes, 2 seconds" (prettyPrint $ 2*60*60 + 2*60 + 2)

test_1_3_days =
    do assertEqual "2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ 2*24*60*60 + 2*60*60 + 2*60 + 2)

test_1_3_months =
    do assertEqual "2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)

test_1_3_years =
    do assertEqual "2 years, 2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ 2*12*30*24*60*60 + 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
