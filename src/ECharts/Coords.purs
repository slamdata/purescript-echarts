module ECharts.Coords where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

import Data.Maybe
import Data.StrMap (fromList)

data XPos = XLeft
          | XRight
          | XCenter
          | X Number


data YPos = YTop
          | YBottom
          | YCenter
          | Y Number


instance xPosShow :: Show XPos where
  show pos = case pos of
    XLeft -> "left"
    XRight -> "right"
    XCenter -> "center"
    X num -> show num

instance yPosShow :: Show YPos where
  show pos = case pos of
    YTop -> "top"
    YBottom -> "bottom"
    YCenter -> "center"
    Y num -> show num


instance xPosEncodeJson :: EncodeJson XPos where
  encodeJson XLeft = fromString "left"
  encodeJson XRight = fromString "right"
  encodeJson XCenter = fromString "center"
  encodeJson (X num) = fromNumber num

instance yPosEncodeJson :: EncodeJson YPos where
  encodeJson ypos = case ypos of
    YTop -> fromString "top"
    YBottom -> fromString "bottom"
    YCenter -> fromString "center"
    Y num -> fromNumber num


data LabelPosition = LPOuter | LPInner | LPTop | LPRight | LPLeft | LPBottom
                   | LPInside
                   | LPInsideLeft | LPInsideRight | LPInsideTop | LPInsideBottom

instance labelPositionShow :: Show LabelPosition where
  show lp = case lp of
    LPOuter -> "outer"
    LPInner -> "inner"
    LPTop -> "top"
    LPRight -> "right"
    LPLeft -> "left"
    LPBottom -> "bottom"
    LPInside -> "inside"
    LPInsideLeft -> "insideLeft"
    LPInsideRight -> "insideRight"
    LPInsideTop -> "insideTop"
    LPInsideBottom -> "insideBottom"

instance labelPositionEncodeJson :: EncodeJson LabelPosition where
  encodeJson = encodeJson <<< show


data HorizontalAlign = HAlignLeft
                     | HAlignRight
                     | HAlignCenter

instance textAlignShow :: Show HorizontalAlign where
  show ta = case ta of
    HAlignLeft -> "left"
    HAlignRight -> "right"
    HAlignCenter -> "center"

instance textAlignEncodeJson :: EncodeJson HorizontalAlign where
  encodeJson = fromString <<< show


newtype Location =
  Location {
    x :: Maybe XPos,
    y :: Maybe YPos
    }


instance locationEncodeJson :: EncodeJson Location where
  encodeJson (Location xy) = fromObject $ fromList $ [
    "x" := xy.x,
    "y" := xy.y
    ]


data Orient = Horizontal | Vertical

instance orientEncodeJson :: EncodeJson Orient where
  encodeJson a = encodeJson $ case a of
    Horizontal -> "horizontal"
    Vertical -> "vertical"
