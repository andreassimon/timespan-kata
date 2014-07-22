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

-- test_1_3_years =
--     do assertEqual "2 years, 2 months, 2 days, 2 hours, 2 minutes, 2 seconds" (prettyPrint LongForm $ 2*12*30*24*60*60 + 2*30*24*60*60 + 2*24*60*60 + 2*60*60 + 2*60 + 2)

-- test_1_4 =
--      do assertEqual "1 second" (prettyPrint BriefForm $ 1)
--         assertEqual "1 day, 1 hour" (prettyPrint BriefForm $ 90012)

data Timespan = Seconds Integer |
                Minutes Integer |
                Hours Integer   |
                Days Integer deriving Eq

showTimespan :: String -> Integer -> String
showTimespan w 1 = "1 " ++ w
showTimespan w v = (show v) ++ " " ++ w ++ "s"

instance Show Timespan where
  show (Seconds s) = showTimespan "second" s
  show (Minutes m) = showTimespan "minute" m
  show (Hours h)   = showTimespan "hour" h
  show (Days d)    = showTimespan "day" d

prettyPrint :: [Timespan] -> String
prettyPrint ts = foldl1 (\x y -> concat [x,", ",y]) $ map show ts

simplify :: Timespan -> [Timespan]
simplify (Seconds seconds) = simplifyL [Seconds seconds]

simplifyL :: [Timespan] -> [Timespan]
simplifyL ((Hours hours):ts)
  | hours < 24   = (Hours hours):ts
  | otherwise    = (Days (hours `div` 24)):(Hours (hours `mod` 24)):ts
simplifyL ((Minutes minutes):ts)
  | minutes < 60 = (Minutes minutes):ts
  | otherwise    = simplifyL $ (Hours (minutes `div` 60)):(Minutes (minutes `mod` 60)):ts
simplifyL [(Seconds seconds)]
  | seconds < 60 = [(Seconds seconds)]
  | otherwise    = simplifyL [(Minutes (seconds `div` 60)), (Seconds (seconds `mod` 60))]


main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
