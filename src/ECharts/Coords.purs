module ECharts.Coords where

import ECharts.Prelude

import Data.StrMap as SM

data XPos
  = XLeft
  | XRight
  | XCenter
  | X Number

instance xPosEncodeJson ∷ EncodeJson XPos where
  encodeJson XLeft = encodeJson "left"
  encodeJson XRight = encodeJson "right"
  encodeJson XCenter = encodeJson "center"
  encodeJson (X num) = encodeJson num

instance xPosDecodeJson ∷ DecodeJson XPos where
  decodeJson j =
    (do str ← decodeJson j
        case str of
          "left" → pure XLeft
          "right" → pure XRight
          "center" → pure XCenter
          _ → Left "incorrect x pos")
    <|> (X <$> decodeJson j)


data YPos
  = YTop
  | YBottom
  | YCenter
  | Y Number

instance yPosEncodeJson ∷ EncodeJson YPos where
  encodeJson ypos = case ypos of
    YTop → encodeJson "top"
    YBottom → encodeJson "bottom"
    YCenter → encodeJson "center"
    Y num → encodeJson num

instance yPosDecodeJson ∷ DecodeJson YPos where
  decodeJson j =
    (do str ← decodeJson j
        case str of
          "top" → pure YTop
          "bottom" → pure YBottom
          "center" → pure YCenter
          _ → Left "incorrect y pos")
    <|> (Y <$> decodeJson j)

data LabelPosition
  = LPOuter
  | LPInner
  | LPTop
  | LPRight
  | LPLeft
  | LPBottom
  | LPInside
  | LPInsideLeft
  | LPInsideRight
  | LPInsideTop
  | LPInsideBottom

instance labelPositionEncodeJson ∷ EncodeJson LabelPosition where
  encodeJson a = encodeJson $ case a of
    LPOuter → "outer"
    LPInner → "inner"
    LPTop → "top"
    LPRight → "right"
    LPLeft → "left"
    LPBottom → "bottom"
    LPInside → "inside"
    LPInsideLeft → "insideLeft"
    LPInsideRight → "insideRight"
    LPInsideTop → "insideTop"
    LPInsideBottom → "insideBottom"

instance labelPositionDecodeJson ∷ DecodeJson LabelPosition where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "outer" → pure LPOuter
      "inner" → pure LPInner
      "top" → pure LPTop
      "right" → pure LPRight
      "left" → pure LPLeft
      "bottom" → pure LPBottom
      "inside" → pure LPInside
      "insideLeft" → pure LPInsideLeft
      "insideRight" → pure LPInsideRight
      "insideTop" → pure LPInsideTop
      "insideBottom" → pure LPInsideBottom
      _ → Left "Invalid LabelPosition"


data HorizontalAlign
  = HAlignLeft
  | HAlignRight
  | HAlignCenter

instance textAlignEncodeJson ∷ EncodeJson HorizontalAlign where
  encodeJson a = encodeJson $ case a of
    HAlignLeft → "left"
    HAlignRight → "right"
    HAlignCenter → "center"


instance textAlignDecodeJson ∷ DecodeJson HorizontalAlign where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "left" → pure HAlignLeft
      "right" → pure HAlignRight
      "center" → pure HAlignCenter
      _ → Left "incorrect text align"


type LocationRec =
  { x ∷ Maybe XPos
  , y ∷ Maybe YPos
  }

newtype Location
  = Location LocationRec

instance locationEncodeJson ∷ EncodeJson Location where
  encodeJson (Location xy) =
    encodeJson
      $ SM.fromFoldable
        [ "x" := xy.x
        , "y" := xy.y
        ]

instance locationDecodeJson ∷ DecodeJson Location where
  decodeJson j = do
    o ← decodeJson j
    r ← {x: _, y: _} <$> (o .? "x") <*> (o .? "y")
    pure $ Location r

data Orient
  = Horizontal
  | Vertical

instance orientEncodeJson ∷ EncodeJson Orient where
  encodeJson a = encodeJson $ case a of
    Horizontal → "horizontal"
    Vertical → "vertical"

instance orientDecodeJson ∷ DecodeJson Orient where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "horizontal" → pure Horizontal
      "vertical" → pure Vertical
      _ → Left "incorrect orient"
