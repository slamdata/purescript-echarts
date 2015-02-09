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
            
instance xPosEncodeJson :: EncodeJson XPos where
  encodeJson XLeft = fromString "left"
  encodeJson XRight = fromString "right"
  encodeJson XCenter = fromString "center"
  encodeJson (X num) = fromNumber num


data YPos = YTop
          | YBottom
          | YCenter
          | Y Number
            
instance yPosEncodeJson :: EncodeJson YPos where
  encodeJson ypos = case ypos of
    YTop -> fromString "top"
    YBottom -> fromString "bottom"
    YCenter -> fromString "center"
    Y num -> fromNumber num


data LabelPosition = LPOuter | LPInner | LPTop | LPRight | LPLeft | LPBottom
                   | LPInside
                   | LPInsideLeft | LPInsideRight | LPInsideTop | LPInsideBottom

instance labelPositionEncodeJson :: EncodeJson LabelPosition where
  encodeJson a = encodeJson $ case a of 
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


data HorizontalAlign = HAlignLeft
                     | HAlignRight
                     | HAlignCenter

instance textAlignEncodeJson :: EncodeJson HorizontalAlign where
  encodeJson a = fromString $ case a of 
    HAlignLeft -> "left"
    HAlignRight -> "right"
    HAlignCenter -> "center"


type LocationRec = {
    x :: Maybe XPos,
    y :: Maybe YPos
    }

newtype Location = Location LocationRec

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
