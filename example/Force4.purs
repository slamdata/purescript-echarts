module Force4 where

import Control.Monad.Eff
import Control.Monad.Eff.Random

import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Common
import ECharts.Style.Item
import ECharts.Series.Force
import ECharts.Color



import Data.Maybe
import Data.Array hiding (init)
import Data.Tuple
import Data.Tuple.Nested
import Data.Foldable
import Data.Traversable
import Utils

type EFLink =
  {
    source :: String,
    target :: String,
    weight :: Number
  }

type EFNode =
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


constMinRadius = 2
constMaxRadius = 10
constMaxDepth = 4
constMinChildren = 2
constMaxChildren = 3


foreign import clientWidth """
function clientWidth(id) {
  return function() {
    return document.getElementById(id).clientWidth;
  };
}
""" :: forall e. String -> Eff e Number

foreign import clientHeight """
function clientHeight(id) {
  return function() {
    return document.getElementById(id).clientHeight;
  };
}
""" :: forall e. String -> Eff e Number

data MockData = MockData [EFNode] [EFLink]

randomInRange min max = 
  (\x -> (max - min) * x + min) <$> random

createRootNode :: forall e. Number -> Eff (random :: Random|e) EFNode
createRootNode depth = do
  width <- clientWidth "force4"
  height <- clientHeight "force4"
  rnd <- random
  rValue <- randomInRange constMinRadius constMaxRadius
  let x = width / 2 + (0.5 - rnd) * 200
  let y = (height - 20) * depth / (constMaxDepth + 1) + 20
  
  return $
    {
      name: "ROOT_NODE",
      value:  rValue,
      id: "root",
      depth: depth,
      initial: x /\ y,
      fixX: false,
      fixY: true,
      category: 2
    }

    
createNodeWithIndex :: forall e. String -> Number -> Eff (random :: Random|e) EFNode
createNodeWithIndex idx depth = do
  node <- createRootNode depth
  return $ node{id = idx,
                name = "NODE_" <> idx,
                category = if depth == constMaxDepth then 0 else 1}
               

mkChild :: forall e. EFNode -> String ->
           Eff (random::Random|e) (Tuple EFNode EFLink)
mkChild node idx = do 
  child <- createNodeWithIndex idx (node.depth + 1)
  let link = {source: node.name, target: child.name, weight: 1}
  return $ Tuple child link

mkChildren :: forall e. EFNode -> Eff (random::Random|e) [Tuple EFNode EFLink]
mkChildren node = do
  childrenCount <- randomInRange constMinChildren constMaxChildren
  let ids = (\x -> node.id <> ":" <> show x) <$> (1..childrenCount)
  sequence $ mkChild node <$> ids


forceMockThreeDataI :: [Tuple EFNode EFLink] -> MockData ->
                       Number -> Eff _ MockData 
forceMockThreeDataI current accum 0 = return accum
forceMockThreeDataI current accum n = do
  let nextAccum = foldl (\(MockData nodes links) (Tuple node link) ->
                          MockData (node:nodes) (link:links)) accum current
  children <- sequence $ mkChildren <<< fst <$> current

  forceMockThreeDataI (concat children) nextAccum (n - 1)


forceMockThreeData :: Eff _ MockData
forceMockThreeData = do
  root <- createRootNode 0
  children <- mkChildren root
  forceMockThreeDataI children (MockData [root] []) constMaxDepth

nodes :: Eff _ _ 
nodes = return []
links :: Eff _ _
links = return []

nodeNormalize :: EFNode -> Node
nodeNormalize node = Node $ (nodeDefault node.value) {
  "name" = Just node.name,
  "initial" = Just node.initial,
  "fixX" = Just  node.fixX,
  "fixY" = Just node.fixY,
  "category" = Just $ node.category
  }


linkNormalize link = Link {
  source: Name link.source,
  target: Name link.target,
  weight: link.weight,
  itemStyle: Nothing
  }

itemColor color = Just $ ItemStyle itemStyleDefault {
  "normal" = Just $ IStyle istyleDefault {
     "color" = Just $ SimpleColor color
     }
  }

mkOptions nodes links = Option $ optionDefault {
  "series" = Just $ Just <$> [
     ForceSeries {
        "common": universalSeriesDefault{
           "name" = Just "Force tree"
           },
        "special": forceSeriesDefault {
          "ribbonType" = Just true,
          "categories" = Just [
            ForceCategory $ forceCategoryDefault {
               "name" = Just "first",
               "itemStyle" = itemColor "#ff7f50"
               },
            ForceCategory $ forceCategoryDefault {
              "name" = Just "second",
              "itemStyle" = itemColor "#6f57bc"
              },
            ForceCategory $ forceCategoryDefault {
              "name" = Just "third",
              "itemStyle" = itemColor "#af0000"
              }
            ],
          "nodes" = Just $ nodeNormalize <$> nodes,
          "links" = Just $ linkNormalize <$> links,
          "minRadius" = Just constMinRadius,
          "maxRadius" = Just constMaxRadius
          }
        
        }
     ]
  }

  


options :: forall e. Eff (random::Random|e) _
options = do
  mockdata <- forceMockThreeData 
  case mockdata of
    MockData nodes links -> do
      log links
      return $ mkOptions nodes links

force4 id = do
  opts <- options
  chart <- getElementById id
           >>= init Nothing
           >>= setOption opts true

  return unit
