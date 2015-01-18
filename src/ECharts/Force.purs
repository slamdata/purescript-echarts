module ECharts.Force where
import Data.Tuple

type Link =
  {
    source :: String,
    target :: String,
    weight :: Number
  }

type Node =
  {
    value :: Number,
    id :: String,
    name :: String,
    depth :: Number,
    initial :: Tuple Number Number,
    fixY :: Boolean,
    fixX :: Boolean,
    category :: Number 
  }
