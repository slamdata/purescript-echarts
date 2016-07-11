module ECharts.Utils where

import Prelude
import Data.Argonaut.Core
import Data.Array (replicate)
import Data.Date as DT
import Data.Date.UTC
import Data.String
import Data.Maybe
import Data.Int as Int
import Math


{-
This func is used to construct copy of object, without null and undefined fields.
i.e
{foo: 1, bar: 12, baz: null, quux: undefined} ->
{foo: 1, bar: 12}
-}
foreign import unnull :: Json -> Json


{-
This func is used to format date string according to some template.
Supported Templates:
"YYYY": 2000 
"YYYY-MMM": 2000-Jan
"YYYY-MM": 2000-01
"MMM-DD": Jan-01
"MM-DD": 01-01
"YYYY-MMM-DD": 2000-Jan-01
"YYYY-MM-DD": 2000-01-01
"MMM/DD/YYYY": Jan/01/2000
-}
dateTimeFormatter :: String -> (String -> String)
dateTimeFormatter template = 
  case template of
    "YYYY" -> converter_YYYY
    "YYYY-MMM" -> converter_YYYYdMMM
    "YYYY-MM" -> converter_YYYYdMM
    "MMM-DD" -> converter_MMMdDD
    "MM-DD" -> converter_MMdDD
    "YYYY-MMM-DD" -> converter_YYYYdMMMdDD
    "YYYY-MM-DD" -> converter_YYYYdMMdDD
    "MMM/DD/YYYY" -> converter_MMMsDDsYYYY
    _ -> doNothing
  
  where
  converter_YYYY :: String -> String
  converter_YYYY dateTime = 
    case DT.fromString dateTime of
      Just dt -> replace ")" "" (replace "(Year " "" (show $ year dt))
      _ -> dateTime

  converter_YYYYdMMM :: String -> String
  converter_YYYYdMMM dateTime = 
    case DT.fromString dateTime of
      Just dt -> (replace ")" "" (replace "(Year " "" (show $ year dt))) ++ "-" ++
        (take 3 (show $ month dt))
      _ -> dateTime

  converter_YYYYdMM :: String -> String
  converter_YYYYdMM dateTime = 
    case DT.fromString dateTime of
      Just dt -> replace ")" "" (replace "(Year " "" (show $ year dt)) ++ "-" ++
        (monthNum $ month dt)
      _ -> dateTime

  converter_MMMdDD :: String -> String
  converter_MMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> (take 3 (show $ month dt)) ++ "-" ++
        (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  converter_MMdDD :: String -> String
  converter_MMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> (monthNum $ month dt) ++ 
      "-" ++ (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  converter_YYYYdMMMdDD :: String -> String
  converter_YYYYdMMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> (replace ")" "" (replace "(Year " "" (show $ year dt))) ++ "-" ++
        (take 3 (show $ month dt)) ++ "-" ++
        (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  converter_YYYYdMMdDD :: String -> String
  converter_YYYYdMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> replace ")" "" (replace "(Year " "" (show $ year dt)) ++ "-" ++
        (monthNum $ month dt) ++ 
        "-" ++ (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  converter_MMMsDDsYYYY :: String -> String
  converter_MMMsDDsYYYY dateTime = 
    case DT.fromString dateTime of
      Just dt -> (take 3 (show $ month dt)) ++ "/" ++
      	(replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt))) ++ "/" ++
      	(replace ")" "" (replace "(Year " "" (show $ year dt))) 
      _ -> dateTime

  monthNum :: DT.Month -> String
  monthNum m = case m of
    DT.January -> "01"
    DT.February -> "02"
    DT.March -> "03"
    DT.April -> "04"
    DT.May -> "05"
    DT.June -> "06"
    DT.July -> "07"
    DT.August -> "08"
    DT.September -> "09"
    DT.October -> "10"
    DT.November -> "11"
    DT.December -> "12"

  doNothing :: String -> String
  doNothing dateTime = dateTime


{-
This func is used to format numeral string accoridng to some template.
Supported Templates:
"0": 1
"0.0": 1.1
"0.00": 1.12
"0.000": 1.123
"0.0000": 1.1234
"0e": 1e+1
"0.0e": 1.1e+1
"0.00e" 1.12e+1
"0a": 1K
"0.0a": 1.1K
"0.00a": 1.12K
"0%": 1%
"0.0%": 1.1%
"0.00%": 1.12%
-}
numeralFormatter :: String -> (Number -> String)
numeralFormatter template =
  case template of
    "0" -> converter_0
    "0.0" -> converter_0d0
    "0.00" -> converter_0d00
    "0.000" -> converter_0d000
    "0.0000" -> converter_0d0000
    "0e" -> converter_0e
    "0.0e" -> converter_0d0e
    "0.00e" -> converter_0d00e
    "0a" -> converter_0a
    "0.0a" -> converter_0d0a
    "0.00a" -> converter_0d00a
    "0%" -> converter_0p
    "0.0%" -> converter_0d0p
    "0.00%" -> converter_0d00p
    _ -> doNothing
  
  where
  converter_0 :: Number -> String
  converter_0 = precision 0 
  
  converter_0d0 :: Number -> String
  converter_0d0 = precision 1
    
  converter_0d00 :: Number -> String
  converter_0d00 = precision 2

  converter_0d000 :: Number -> String
  converter_0d000 = precision 3
  
  converter_0d0000 :: Number -> String
  converter_0d0000 = precision 4

  converter_0e :: Number -> String
  converter_0e = converter_e 0

  converter_0d0e :: Number -> String
  converter_0d0e = converter_e 1

  converter_0d00e :: Number -> String
  converter_0d00e = converter_e 2

  converter_0a :: Number -> String
  converter_0a = converter_a 0

  converter_0d0a :: Number -> String
  converter_0d0a = converter_a 1

  converter_0d00a :: Number -> String
  converter_0d00a = converter_a 2

  converter_0p :: Number -> String
  converter_0p = converter_p 0

  converter_0d0p :: Number -> String
  converter_0d0p = converter_p 1

  converter_0d00p :: Number -> String
  converter_0d00p = converter_p 2

  precision :: Int -> Number -> String
  precision p num = case p of 
    0 -> show $ Int.round num
    _ -> numStr ++ 
          fromCharArray (replicate (p - length (dropWhile isNotDot numStr) + 1) '0')
      where 
      f = 10.0 `pow` Int.toNumber p
      numStr = show (round (num * f) / f)

  converter_e :: Int -> Number -> String
  converter_e p num = case num == 0.0 of
    true -> precision p num
    false -> case d1 > d2 of
      true -> 
        precision p (num / (10.0 `pow` Int.toNumber (d1-1))) ++ 
          "e+" ++ show (d1-1)
      false ->
        precision p (num * (10.0 `pow` Int.toNumber (d2-1))) ++ 
          "e-" ++ show (d2-1)
    where
    numAbsStr = show $ abs num
    d1 = length (takeWhile isNotDot numAbsStr)
    d2 = length (takeWhile isZeroOrDot numAbsStr)

  converter_a :: Int -> Number -> String
  converter_a p num = case (d - 1)/3 of
    0 -> precision p num
    1 -> precision p (num/(1000.0 `pow` 1.0)) ++ "K"
    2 -> precision p (num/(1000.0 `pow` 2.0)) ++ "M"
    3 -> precision p (num/(1000.0 `pow` 3.0)) ++ "B"
    4 -> precision p (num/(1000.0 `pow` 4.0)) ++ "T"
    _ -> precision p num
    where
    numAbsStr = show $ abs num
    d = length (takeWhile isNotDot numAbsStr)

  converter_p :: Int -> Number -> String
  converter_p p num = precision p (num * 100.0) ++ "%"

  doNothing :: Number -> String
  doNothing num = show num

  isNotDot :: Char -> Boolean
  isNotDot c = c /= '.' 

  isZeroOrDot :: Char -> Boolean
  isZeroOrDot c = c == '0' || c == '.'