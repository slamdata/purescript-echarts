module Force4 where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import ECharts.Chart
import ECharts.Options.Unsafe

import DOM 
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

mkOptions nodes links = 
 {
    "title":
    {
      "text": "Force",
      "subtext": "Force-directed tree",
      "x": "right",
      "y": "bottom"
    },
    "tooltip":
    {
      "trigger": "item",
      "formatter": "{a} : {b}"
    },
    "toolbox":
    {
      "show": true,
      "feature":
      {
        "restore": {"show": true},
        "magicType": {"show": true, "type": ["force", "chord"]},
        "saveAsImage": {"show" :true}
      }
    },
    "legend":
    {
      "x": "left",
      "data": ["叶子节点","非叶子节点", "根节点"]
    },
    "series":
    [{
      "type": "force",
      "name": "Force tree",
      "ribbonType": false,
      "categories":
      (
        {
          "name": "叶子节点",
          "itemStyle": {"normal": {"color": "#ff7f50"}}
        } /\
        {
          "name": "非叶子节点",
          "itemStyle": {"normal": {"color": "#6f57bc"}}
        } /\
        {
          "name": "根节点",
          "itemStyle": {"normal": {"color": "af0000"}}
        }
      ),
      "itemStyle":
      {
        "normal":
        {
          "label": {"show": false},
          "nodeStyle":
          {
            "brushType": "both",
            "strokeColor": "rgba(255, 215, 0, 0.6)",
            "lineWidth": 1
          }
        }
      },
      "minRadius": constMinRadius,
      "maxRadius": constMaxRadius,
      "nodes": nodes,
      "links": links
      
    }]
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
           >>= setOptionUnsafe opts true

  return unit
