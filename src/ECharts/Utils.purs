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
        case month dt of
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
      Just dt ->
        case month dt of
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
        ++ "-" ++ (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
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
        case month dt of
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
        ++ "-" ++ (replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt)))
      _ -> dateTime

  converter_MMMsDDsYYYY :: String -> String
  converter_MMMsDDsYYYY dateTime = 
    case DT.fromString dateTime of
      Just dt -> (take 3 (show $ month dt)) ++ "/" ++
      	(replace ")" "" (replace "(DayOfMonth " "" (show $ dayOfMonth dt))) ++ "/" ++
      	(replace ")" "" (replace "(Year " "" (show $ year dt))) 
      _ -> dateTime

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
numeralFormatter template = numeralFormatterWithValMnplt 0.0 1.0 template


{-
This func can
1) manipluate numerical values by adding and then multiplying
2) format the manipulated value to a string according to some template
-}
numeralFormatterWithValMnplt :: Number -> Number -> String -> (Number -> String)
numeralFormatterWithValMnplt add mul template = 
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
  converter_0 num = show $ Int.round ((num * 1.0 + add) * mul)
  
  converter_0d0 :: Number -> String
  converter_0d0 num = 
    numStr ++ 
    	fromCharArray (replicate (p - length (dropWhile isNotDot numStr) + 1) '0')
    where 
    p = 1
    f = 10.0 `pow` Int.toNumber p
    numStr = show (round (((num * 1.0 + add) * mul) * f) / f)
  
  converter_0d00 :: Number -> String
  converter_0d00 num = 
    numStr ++ 
    	fromCharArray (replicate (p - length (dropWhile isNotDot numStr) + 1) '0')
    where 
    p = 2
    f = 10.0 `pow` Int.toNumber p
    numStr = show (round (((num * 1.0 + add) * mul) * f) / f)

  converter_0d000 :: Number -> String
  converter_0d000 num = 
    numStr ++ 
    	fromCharArray (replicate (p - length (dropWhile isNotDot numStr) + 1) '0')
    where 
    p = 3
    f = 10.0 `pow` Int.toNumber p
    numStr = show (round (((num * 1.0 + add) * mul) * f) / f)
  
  converter_0d0000 :: Number -> String
  converter_0d0000 num = 
    numStr ++ 
    	fromCharArray (replicate (p - length (dropWhile isNotDot numStr) + 1) '0')
    where 
    p = 4
    f = 10.0 `pow` Int.toNumber p
    numStr = show (round (((num * 1.0 + add) * mul) * f) / f)

  converter_0e :: Number -> String
  converter_0e num = case ((num * 1.0 + add) * mul) == 0.0 of
  	true -> converter_0 ((num * 1.0 + add) * mul)
  	false -> case d1 > d2 of
  		true -> 
  			(converter_0 (((num * 1.0 + add) * mul) / (10.0 `pow` Int.toNumber (d1-1)))) ++ 
  				"e+" ++ show (d1-1)
  		false ->
  			(converter_0 (((num * 1.0 + add) * mul) * (10.0 `pow` Int.toNumber (d2-1)))) ++ 
  				"e-" ++ show (d2-1)
    where
    numAbsStr = show (abs ((num * 1.0 + add) * mul))
    d1 = length (takeWhile isNotDot numAbsStr)
    d2 = length (takeWhile isZeroOrDot numAbsStr)

  converter_0d0e :: Number -> String
  converter_0d0e num = case ((num * 1.0 + add) * mul) == 0.0 of
  	true -> converter_0d0 ((num * 1.0 + add) * mul)
  	false -> case d1 > d2 of
  		true -> 
  			(converter_0d0 (((num * 1.0 + add) * mul) / (10.0 `pow` Int.toNumber (d1-1)))) ++ 
  				"e+" ++ show (d1-1)
  		false ->
  			(converter_0d0 (((num * 1.0 + add) * mul) * (10.0 `pow` Int.toNumber (d2-1)))) ++ 
  				"e-" ++ show (d2-1)
    where
    numAbsStr = show (abs ((num * 1.0 + add) * mul))
    d1 = length (takeWhile isNotDot numAbsStr)
    d2 = length (takeWhile isZeroOrDot numAbsStr)

  converter_0d00e :: Number -> String
  converter_0d00e num = case ((num * 1.0 + add) * mul) == 0.0 of
  	true -> converter_0d00 ((num * 1.0 + add) * mul)
  	false -> case d1 > d2 of
  		true -> 
  			(converter_0d00 (((num * 1.0 + add) * mul) / (10.0 `pow` Int.toNumber (d1-1)))) ++ 
  				"e+" ++ show (d1-1)
  		false ->
  			(converter_0d00 (((num * 1.0 + add) * mul) * (10.0 `pow` Int.toNumber (d2-1)))) ++ 
  				"e-" ++ show (d2-1)
    where
    numAbsStr = show (abs ((num * 1.0 + add) * mul))
    d1 = length (takeWhile isNotDot numAbsStr)
    d2 = length (takeWhile isZeroOrDot numAbsStr)

  converter_0a :: Number -> String
  converter_0a num = case (d - 1)/3 of
    0 -> converter_0 ((num * 1.0 + add) * mul)
    1 -> (converter_0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 1.0))) ++ "K"
    2 -> (converter_0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 2.0))) ++ "M"
    3 -> (converter_0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 3.0))) ++ "B"
    4 -> (converter_0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 4.0))) ++ "T"
    _ -> converter_0e ((num * 1.0 + add) * mul)
    where
    numAbsStr = show (abs ((num * 1.0 + add) * mul))
    d = length (takeWhile isNotDot numAbsStr)

  converter_0d0a :: Number -> String
  converter_0d0a num = case (d - 1)/3 of
    0 -> converter_0d0 ((num * 1.0 + add) * mul)
    1 -> (converter_0d0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 1.0))) ++ "K"
    2 -> (converter_0d0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 2.0))) ++ "M"
    3 -> (converter_0d0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 3.0))) ++ "B"
    4 -> (converter_0d0 (((num * 1.0 + add) * mul)/(1000.0 `pow` 4.0))) ++ "T"
    _ -> converter_0d0e ((num * 1.0 + add) * mul)
    where
    numAbsStr = show (abs ((num * 1.0 + add) * mul))
    d = length (takeWhile isNotDot numAbsStr)

  converter_0d00a :: Number -> String
  converter_0d00a num = case (d - 1)/3 of
    0 -> converter_0d00 ((num * 1.0 + add) * mul)
    1 -> (converter_0d00 (((num * 1.0 + add) * mul)/(1000.0 `pow` 1.0))) ++ "K"
    2 -> (converter_0d00 (((num * 1.0 + add) * mul)/(1000.0 `pow` 2.0))) ++ "M"
    3 -> (converter_0d00 (((num * 1.0 + add) * mul)/(1000.0 `pow` 3.0))) ++ "B"
    4 -> (converter_0d00 (((num * 1.0 + add) * mul)/(1000.0 `pow` 4.0))) ++ "T"
    _ -> converter_0d00e ((num * 1.0 + add) * mul)
    where
    numAbsStr = show (abs ((num * 1.0 + add) * mul))
    d = length (takeWhile isNotDot numAbsStr)

  converter_0p :: Number -> String
  converter_0p num = (converter_0 $ ((num * 1.0 + add) * mul)*100.0) ++ "%"

  converter_0d0p :: Number -> String
  converter_0d0p num = (converter_0d0 $ ((num * 1.0 + add) * mul)*100.0) ++ "%"

  converter_0d00p :: Number -> String
  converter_0d00p num = (converter_0d00 $ ((num * 1.0 + add) * mul)*100.0) ++ "%"

  doNothing :: Number -> String
  doNothing num = show ((num * 1.0 + add) * mul)

  isNotDot :: Char -> Boolean
  isNotDot c = c /= '.' 

  isZeroOrDot :: Char -> Boolean
  isZeroOrDot c = c == '0' || c == '.'
