{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Test.Framework
import Test.Framework.TestManager

data Seconds = Seconds Integer deriving Eq
type Minutes = Integer
type Hours = Integer
data Days = Days Integer deriving Eq
type Months = Integer
data Years = Years Integer deriving Eq

showTime :: String -> Integer -> String
showTime w 1 = "1 " ++ w
showTime w v = (show v) ++ " " ++ w ++ "s"

instance Show Seconds where
  show (Seconds s) = showTime "second" s

instance Show Days where
  show (Days d) = showTime "day" d

instance Show Years where
  show (Years y) = showTime "year" y

data Timespan = Timespan (Seconds, Minutes, Hours, Days, Months, Years)

data Brevity = BriefForm | LongForm

prettyPrint :: Brevity -> Integer -> String
prettyPrint LongForm seconds = _prettyPrint $ simplify seconds
prettyPrint BriefForm seconds = _prettyPrintBriefly 3 $ simplify seconds

class Simplify a where 
  simplify :: a -> Timespan

instance Simplify Integer where
  simplify seconds = simplify (Timespan (Seconds seconds, 0, 0, Days 0, 0, Years 0))

instance Simplify Timespan where 
  simplify (Timespan (Seconds seconds, minutes, hours, Days days, months, Years years))
    | seconds >= 60  =  simplify (Timespan (Seconds (seconds `mod` 60), minutes+(seconds `div` 60), hours, Days days, months, Years years))
    | minutes >= 60  =  simplify (Timespan (Seconds seconds, minutes `mod` 60, hours+(minutes `div` 60), Days days, months, Years years))
    | hours   >= 24  =  simplify (Timespan (Seconds seconds, minutes, hours `mod` 24, Days (days+(hours `div` 24)), months, Years years))
    | days    >= 30  =  simplify (Timespan (Seconds seconds, minutes, hours, Days (days `mod` 30), months+(days `div` 30), Years years))
    | months  >= 12  =  simplify (Timespan (Seconds seconds, minutes, hours, Days days, months `mod` 12, Years (years+(months `div` 12))))
    | otherwise      =  Timespan (Seconds seconds, minutes, hours, Days days, months, Years years)

_prettyPrint :: Timespan -> String
_prettyPrint (Timespan (seconds,               0,     0, Days 0,   0, Years 0)) = (show seconds)
_prettyPrint (Timespan (Seconds seconds, minutes,     0, Days 0,   0, Years 0)) = (show minutes) ++ " minutes, " ++ (_prettyPrint (Timespan (Seconds seconds, 0, 0, Days 0, 0, Years 0)))
_prettyPrint (Timespan (Seconds seconds, minutes, hours, Days 0,   0, Years 0)) = (show hours) ++ " hours, " ++ (_prettyPrint (Timespan (Seconds seconds, minutes, 0, Days 0, 0, Years 0)))
_prettyPrint (Timespan (Seconds seconds, minutes, hours,   days,   0, Years 0)) = (show days) ++ ", " ++ (_prettyPrint (Timespan (Seconds seconds, minutes, hours, Days 0, 0, Years 0)))
_prettyPrint (Timespan (Seconds seconds, minutes, hours,   days, months, Years 0)) = (show months) ++ " months, " ++ (_prettyPrint (Timespan (Seconds seconds, minutes, hours, days, 0, Years 0)))
_prettyPrint (Timespan (Seconds seconds, minutes, hours,   days, months, years)) = (show years) ++ ", " ++ (_prettyPrint (Timespan (Seconds seconds, minutes, hours, days, months, Years 0)))

_prettyPrintBriefly :: Int -> Timespan -> String
_prettyPrintBriefly 0 _ = ""
_prettyPrintBriefly unitsLeft (Timespan (Seconds seconds, minutes, hours, Days days, months, Years years))
--   | years > 0     = (show (Years years)) ++ ", " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, minutes, hours, days, months, Years 0))
--   | months > 1    = (show months) ++ " months, " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, minutes, hours, days, 0, Years 0))
--   | months == 1   = "1 month, " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, minutes, hours, days, 0, Years 0))
  | days > 0      = (show (Days days)) ++ ", " ++ (_prettyPrintBriefly (unitsLeft-1) (Timespan (Seconds seconds, minutes, hours, Days 0, 0, Years 0)))
--   | hours > 1     = (show hours) ++ " hours, " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, minutes, 0, 0, 0, Years 0))
--   | hours == 1    = "1 hour, " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, minutes, 0, 0, 0, Years 0))
--   | minutes > 1   = (show minutes) ++ " minutes, " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, 0, 0, 0, 0, Years 0))
--   | minutes == 0  = "1 minute, " ++ (_prettyPrintBriefly (unitsLeft-1) (Seconds seconds, 0, 0, 0, 0, Years 0))
  | seconds > 0   = (show (Seconds seconds))

-- seconds (s, _, _, _, _, _) = s
-- minutes (_, m, _, _, _, _) = m
-- hours   (_, _, h, _, _, _) = h
-- days    (_, _, _, d, _, _) = d
-- months  (_, _, _, _, m, _) = m
-- years   (_, _, _, _, _, y) = y

test_1_1 =
    do assertEqual "30 seconds" (prettyPrint LongForm 30)
       assertEqual "20 seconds" (prettyPrint LongForm 20)

test_1_2 =
    do assertEqual "2 minutes, 20 seconds" (prettyPrint LongForm 140)
       assertEqual "3 minutes, 0 seconds" (prettyPrint LongForm 180)

-- Implement support for hours (days, months and years (we assume 30 days = 1 month, 1 year = 12 months))
test_1_3_hours =
    do assertEqual "2 hours, 2 minutes, 2 seconds" (prettyPrint LongForm $ 2*60*60 + 2*60 + 2)

test_1_3_days =
    do assertEqual "2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint LongForm $ 2*24*60*60 + 2*60*60 + 2*60 + 2)

test_1_3_months =
    do assertEqual "2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint LongForm $ 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)

test_1_3_years =
    do assertEqual "2 years, 2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint LongForm $ 2*12*30*24*60*60 + 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)

test_1_4 =
     do assertEqual "1 second" (prettyPrint BriefForm $ 1)
        assertEqual "1 day, 1 hour" (prettyPrint BriefForm $ 90012)

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
