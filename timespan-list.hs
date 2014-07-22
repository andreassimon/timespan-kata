{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment

import Test.Framework
import Test.Framework.TestManager


test_1_1 =
    do assertEqual "30 seconds" (prettyPrint $ simplify (Seconds 30))
       assertEqual "20 seconds" (prettyPrint $ simplify (Seconds 20))

test_1_2 =
    do assertEqual "2 minutes, 20 seconds" (prettyPrint $ simplify (Seconds 140))
       assertEqual "3 minutes, 0 seconds" (prettyPrint $ simplify (Seconds 180))

-- -- Implement support for hours (days, months and years (we assume 30 days = 1 month, 1 year = 12 months))
test_1_3_hours =
    do assertEqual "2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplify $ (Seconds (2*60*60 + 2*60 + 2)))

test_1_3_days =
    do assertEqual "2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplify $ (Seconds (2*24*60*60 + 2*60*60 + 2*60 + 2)))

test_1_3_months =
    do assertEqual "2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplify $ (Seconds (2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)))

test_1_3_years =
    do assertEqual "2 years, 2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint $ simplify $ (Seconds (2*12*30*24*60*60 + 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)))

test_2_1 =
     do assertEqual "1 second" (prettyPrintBriefly $ simplify (Seconds 1))
        assertEqual "1 day, 1 hour" (prettyPrintBriefly $ simplify (Seconds 90012))
     where
        prettyPrintBriefly ts = prettyPrint $ filter timespanNotZero $ take 3 ts

test_2_2 =
     do assertEqual "2 days, 0 hours, 1 minute" (prettyPrintBriefly $ simplify (Seconds 172860))
     where
        prettyPrintBriefly ts = prettyPrint $ take 3 ts

test_2_3 =
     do assertEqual "2 days, 0 hours, 1 minute" (prettyPrintBriefly $ simplify (Seconds 172860))
     where
        prettyPrintBriefly ts = prettyPrint $ take 3 ts

data Timespan = Seconds Integer |
                Minutes Integer |
                Hours Integer   |
                Days Integer    |
                Months Integer  |
                Years Integer deriving Eq

showTimespan :: String -> Integer -> String
showTimespan w 1 = "1 " ++ w
showTimespan w v = (show v) ++ " " ++ w ++ "s"

instance Show Timespan where
  show (Seconds s) = showTimespan "second" s
  show (Minutes m) = showTimespan "minute" m
  show (Hours h)   = showTimespan "hour" h
  show (Days d)    = showTimespan "day" d
  show (Months m)  = showTimespan "month" m
  show (Years y)   = showTimespan "year" y

timespanNotZero :: Timespan -> Bool
timespanNotZero (Seconds 0) = False
timespanNotZero (Minutes 0) = False
timespanNotZero (Hours 0)   = False
timespanNotZero (Days 0)    = False
timespanNotZero (Months 0)  = False
timespanNotZero (Years 0)   = False
timespanNotZero _           = True

prettyPrint :: [Timespan] -> String
prettyPrint ts = foldl1 (\x y -> concat [x,", ",y]) $ map show ts

simplify :: Timespan -> [Timespan]
simplify (Seconds seconds) = simplifyL [Seconds seconds]

simplifyL :: [Timespan] -> [Timespan]
simplifyL ((Months months):ts)
  | months < 12  = (Months months):ts
  | otherwise    = simplifyL $ (Years (months `div` 12)):(Months (months `mod` 12)):ts
simplifyL ((Days days):ts)
  | days < 30    = (Days days):ts
  | otherwise    = simplifyL $ (Months (days `div` 30)):(Days (days `mod` 30)):ts
simplifyL ((Hours hours):ts)
  | hours < 24   = (Hours hours):ts
  | otherwise    = simplifyL $ (Days (hours `div` 24)):(Hours (hours `mod` 24)):ts
simplifyL ((Minutes minutes):ts)
  | minutes < 60 = (Minutes minutes):ts
  | otherwise    = simplifyL $ (Hours (minutes `div` 60)):(Minutes (minutes `mod` 60)):ts
simplifyL [(Seconds seconds)]
  | seconds < 60 = [(Seconds seconds)]
  | otherwise    = simplifyL [(Minutes (seconds `div` 60)), (Seconds (seconds `mod` 60))]
simplifyL ts     = ts


main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
