module ECharts.Utils where

import Prelude
import Data.Argonaut.Core
import Data.Date as DT
import Data.Date.UTC
import Data.String
import Data.Maybe


{-
This func is used to construct copy of object, without null and undefined fields.
i.e
{foo: 1, bar: 12, baz: null, quux: undefined} ->
{foo: 1, bar: 12}
-}
foreign import unnull :: Json -> Json

{-
This func is used to format date string arrording to some template.
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



