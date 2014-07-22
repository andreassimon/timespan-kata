{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Data.Char

import Test.Framework
import Test.Framework.TestManager


seconds s = Timespan (Second, s)
simplifySeconds s = Main.Just $ simplify $ seconds s

test_1_1 =
    do assertEqual "30 seconds" (prettyPrint $ simplifySeconds 30)
       assertEqual "20 seconds" (prettyPrint $ simplifySeconds 20)

test_1_2 =
    do assertEqual "2 minutes, 20 seconds" (prettyPrint $ simplifySeconds 140)
       assertEqual "3 minutes, 0 seconds" (prettyPrint $ simplifySeconds 180)

-- -- -- Implement support for hours (days, months and years (we assume 30 days = 1 month, 1 year = 12 months))
test_1_3_hours =
    do assertEqual "2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplifySeconds (2*60*60 + 2*60 + 2))

test_1_3_days =
    do assertEqual "2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplifySeconds (2*24*60*60 + 2*60*60 + 2*60 + 2))

test_1_3_months =
    do assertEqual "2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplifySeconds (2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2))

test_1_3_years =
    do assertEqual "2 years, 2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplifySeconds (2*12*30*24*60*60 + 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2))

test_2_1 =
     do assertEqual "1 second" (prettyPrintBriefly $ simplify $ seconds 1)
        assertEqual "1 day, 1 hour" (prettyPrintBriefly $ simplify $ seconds 90012)
     where
        prettyPrintBriefly ts = prettyPrint $ Main.Just $ filter timespanNotZero $ take 3 ts

test_2_2 =
     do assertEqual "2 days, 0 hours, 1 minute" (prettyPrintBriefly $ simplify $ seconds 172860)
     where
        prettyPrintBriefly ts = prettyPrint $ Main.Just $ take 3 ts

test_2_3 =
     do assertEqual "2 days, 0 hours, 1 minute" (prettyPrintBriefly $ simplify $ seconds 172860)
     where
        prettyPrintBriefly ts = prettyPrint $ Main.Just $ take 3 ts

test_3_1 =
     do assertEqual "2 days" (prettyPrintLossy $ simplify $ seconds 172862)
     where
        prettyPrintLossy ts = prettyPrint $ Main.Just $ take 1 ts

test_3_2 =
     do assertEqual "1 second" (prettyPrintLossy 2 $ simplify $ seconds 1)
        assertEqual "2 seconds" (prettyPrintLossy 2 $ simplify $ seconds (2))
        assertEqual "over 1 minute" (prettyPrintLossy 2 $ simplify $ seconds (1*minute + 29))
        assertEqual "almost 2 minutes" (prettyPrintLossy 2 $ simplify $ seconds (1*minute + 30))
        assertEqual "over 1 hour" (prettyPrintLossy 2 $ simplify $ seconds (1*hour + 29*minutes))
        assertEqual "almost 2 hours" (prettyPrintLossy 2 $ simplify $ seconds (1*hour + 30*minutes))
        assertEqual "over 2 days" (prettyPrintLossy 2 $ simplify $ seconds 198719)
        assertEqual "almost 3 days" (prettyPrintLossy 2 $ simplify $ seconds 233280)
        assertEqual "over 1 month" (prettyPrintLossy 2 $  simplify $ seconds (1*month + 14*days))
        assertEqual "almost 2 months" (prettyPrintLossy 2 $ simplify $ seconds (1*month + 15*days))
        assertEqual "over 1 year" (prettyPrintLossy 2 $ simplify $ seconds (1*year + 5*months))
        assertEqual "almost 2 years" (prettyPrintLossy 2 $ simplify $ seconds (1*year + 6*months))
        assertEqual "over 2 years, 1 month" (prettyPrintLossy 3 $ simplify $ seconds (2*years + 1*month + 14*days))
     where
        year = years
        years = 12*months
        month = months
        months = 30*days
        days = 24*hours
        hour = hours
        hours = 60*minutes
        minute = minutes
        minutes = 60
        prettyPrintLossy n ts = prettyPrint $ Main.round $ take n ts

test_3_3 =
     do assertEqual "over 2 days" (prettyPrintLossy $ simplify $ seconds (198719))
        assertEqual "almost 3 days" (prettyPrintLossy $ simplify $ seconds (233280))
     where
        prettyPrintLossy ts = prettyPrint $ Main.round $ take 2 ts

test_show =
        do assertEqual "1 year" (show $ Timespan (Year, 1))
           assertEqual "2 seconds" (show $ Timespan (Second, 2))

data Timespan = Timespan (TimespanUnit, Value) deriving (Eq)
data TimespanUnit = Second | Minute | Hour | Day | Month | Year deriving (Show, Eq, Ord)
type Value = Integer

instance Show Timespan where
  show (Timespan (unit, 1)) = "1 " ++ (map toLower $ show unit)
  show (Timespan (unit, v)) = (show v) ++ " " ++ (map toLower $ show unit) ++ "s"

timespanNotZero :: Timespan -> Bool
timespanNotZero (Timespan (_, 0)) = False
timespanNotZero _                 = True

prettyPrint :: RoundedTimespan -> String
prettyPrint (Main.Just ts) = foldl1 (\x y -> concat [x,", ",y]) $ map show ts
prettyPrint (Main.Floor ts) = "over " ++ prettyPrint (Main.Just ts)
prettyPrint (Main.Ceiling ts) = "almost " ++ prettyPrint (Main.Just ts)

simplify :: Timespan -> [Timespan]
simplify t@(Timespan (Second, seconds)) = simplifyL [t]

constructTimespan :: TimespanUnit -> (Integer -> Timespan)
constructTimespan unit = \v -> Timespan (unit, v)

nextUnit :: TimespanUnit -> TimespanUnit
nextUnit Second = Minute
nextUnit Minute = Hour
nextUnit Hour = Day
nextUnit Day = Month
nextUnit Month = Year

maxValue :: TimespanUnit -> Integer
maxValue Second = 60
maxValue Minute = 60
maxValue Hour   = 24
maxValue Day    = 30
maxValue Month  = 12

test_simplifyL =
     do assertEqual [Timespan (Minute, 1), Timespan (Second, 1)] (simplifyL [Timespan (Second, 61)])
        assertEqual [Timespan (Hour, 2), Timespan (Minute, 0), Timespan (Second, 1)] (simplifyL [Timespan (Second, 2*hours + 1)])
        assertEqual (Timespan (Year, 2)) (head $ simplifyL [Timespan (Second, 2*years)])
     where
        years = 12*months
        months = 30*days
        days = 24*hours
        hours = 60*minutes
        minutes = 60

simplifyL :: [Timespan] -> [Timespan]
simplifyL (t@(Timespan (unit, value)):ts)
  | exceedsMax t = simplifyL (((constructTimespan $ nextUnit unit) (value `div` max)):((constructTimespan unit) (value `mod` max)):ts)
  where
    exceedsMax (Timespan (Year, value)) = False
    exceedsMax (Timespan (unit, value)) = value > (maxValue unit)
    max = maxValue unit
simplifyL ts      = ts

data RoundedTimespan = Floor [Timespan] |
                       Ceiling [Timespan] |
                       Just [Timespan] deriving (Eq, Show)

round :: [Timespan] -> RoundedTimespan
round [Timespan (Second, s)] = Main.Just [Timespan (Second, s)]
round [Timespan (t1, v1), Timespan (t2, v2)]
  | t1 == nextUnit t2 && v2 < ((maxValue t2) `div` 2) = Floor [Timespan (t1, v1)]
  | otherwise = Ceiling [Timespan (t1, (v1+1))]
round (t:ts) = (cc t (Main.round ts))
  where cc t (Floor ts) = Floor (t:ts)
        cc t (Ceiling ts) = Ceiling (t:ts)


main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
