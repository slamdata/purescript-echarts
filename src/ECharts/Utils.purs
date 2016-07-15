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
    "YYYY" -> formatAsYYYY
    "YYYY-MMM" -> formatAsYYYYdMMM
    "YYYY-MM" -> formatAsYYYYdMM
    "MMM-DD" -> formatAsMMMdDD
    "MM-DD" -> formatAsMMdDD
    "YYYY-MMM-DD" -> formatAsYYYYdMMMdDD
    "YYYY-MM-DD" -> formatAsYYYYdMMdDD
    "MMM/DD/YYYY" -> formatAsMMMsDDsYYYY
    _ -> doNothing
  
  where
  formatAsYYYY :: String -> String
  formatAsYYYY dateTime = 
    case DT.fromString dateTime of
      Just dt -> replace ")" "" (replace "(Year " "" (show $ year dt))
      _ -> dateTime

  formatAsYYYYdMMM :: String -> String
  formatAsYYYYdMMM dateTime = 
    case DT.fromString dateTime of
      Just dt -> (replace ")" "" (replace "(Year " "" (show $ year dt))) ++ "-" ++
        (take 3 (show $ month dt))
      _ -> dateTime

  formatAsYYYYdMM :: String -> String
  formatAsYYYYdMM dateTime = 
    case DT.fromString dateTime of
      Just dt -> replace ")" "" (replace "(Year " "" (show $ year dt)) ++ "-" ++
        (monthNum $ month dt)
      _ -> dateTime

  formatAsMMMdDD :: String -> String
  formatAsMMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> (take 3 (show $ month dt)) ++ "-" ++
        (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  formatAsMMdDD :: String -> String
  formatAsMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> (monthNum $ month dt) ++ 
      "-" ++ (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  formatAsYYYYdMMMdDD :: String -> String
  formatAsYYYYdMMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> (replace ")" "" (replace "(Year " "" (show $ year dt))) ++ "-" ++
        (take 3 (show $ month dt)) ++ "-" ++
        (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  formatAsYYYYdMMdDD :: String -> String
  formatAsYYYYdMMdDD dateTime = 
    case DT.fromString dateTime of
      Just dt -> replace ")" "" (replace "(Year " "" (show $ year dt)) ++ "-" ++
        (monthNum $ month dt) ++ 
        "-" ++ (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  formatAsMMMsDDsYYYY :: String -> String
  formatAsMMMsDDsYYYY dateTime = 
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
    "0" -> formatAs0
    "0.0" -> formatAs0d0
    "0.00" -> formatAs0d00
    "0.000" -> formatAs0d000
    "0.0000" -> formatAs0d0000
    "0e" -> formatAs0e
    "0.0e" -> formatAs0d0e
    "0.00e" -> formatAs0d00e
    "0a" -> formatAs0a
    "0.0a" -> formatAs0d0a
    "0.00a" -> formatAs0d00a
    "0%" -> formatAs0p
    "0.0%" -> formatAs0d0p
    "0.00%" -> formatAs0d00p
    _ -> doNothing
  
  where
  formatAs0 :: Number -> String
  formatAs0 = precision 0 
  
  formatAs0d0 :: Number -> String
  formatAs0d0 = precision 1
    
  formatAs0d00 :: Number -> String
  formatAs0d00 = precision 2

  formatAs0d000 :: Number -> String
  formatAs0d000 = precision 3
  
  formatAs0d0000 :: Number -> String
  formatAs0d0000 = precision 4

  formatAs0e :: Number -> String
  formatAs0e = formatAsE 0

  formatAs0d0e :: Number -> String
  formatAs0d0e = formatAsE 1

  formatAs0d00e :: Number -> String
  formatAs0d00e = formatAsE 2

  formatAs0a :: Number -> String
  formatAs0a = formatAsA 0

  formatAs0d0a :: Number -> String
  formatAs0d0a = formatAsA 1

  formatAs0d00a :: Number -> String
  formatAs0d00a = formatAsA 2

  formatAs0p :: Number -> String
  formatAs0p = formatAsP 0

  formatAs0d0p :: Number -> String
  formatAs0d0p = formatAsP 1

  formatAs0d00p :: Number -> String
  formatAs0d00p = formatAsP 2

  precision :: Int -> Number -> String
  precision p num = case p of 
    0 -> show $ Int.round num
    _ -> numStr ++ 
          fromCharArray (replicate (p - length (dropWhile isNotDot numStr) + 1) '0')
      where 
      f = 10.0 `pow` Int.toNumber p
      numStr = show (round (num * f) / f)

  formatAsE :: Int -> Number -> String
  formatAsE p num = case num == 0.0 of
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

  formatAsA :: Int -> Number -> String
  formatAsA p num = case (d - 1)/3 of
    0 -> precision p num
    1 -> precision p (num/(1000.0 `pow` 1.0)) ++ "K"
    2 -> precision p (num/(1000.0 `pow` 2.0)) ++ "M"
    3 -> precision p (num/(1000.0 `pow` 3.0)) ++ "B"
    4 -> precision p (num/(1000.0 `pow` 4.0)) ++ "T"
    _ -> precision p num
    where
    numAbsStr = show $ abs num
    d = length (takeWhile isNotDot numAbsStr)

  formatAsP :: Int -> Number -> String
  formatAsP p num = precision p (num * 100.0) ++ "%"

  doNothing :: Number -> String
  doNothing num = show num

  isNotDot :: Char -> Boolean
  isNotDot c = c /= '.' 

  isZeroOrDot :: Char -> Boolean
  isZeroOrDot c = c == '0' || c == '.'