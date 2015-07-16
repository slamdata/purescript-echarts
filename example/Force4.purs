module Force4 where

import Prelude
import Control.Monad.Eff.Console (print)
import Control.Monad.Eff
import Control.Monad.Eff.Random

import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Common
import ECharts.Style.Item
import ECharts.Series.Force
import ECharts.Color


import Data.Int (toNumber, fromNumber)
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
    depth :: Int,
    initial :: Tuple Number Number,
    fixY :: Boolean,
    fixX :: Boolean,
    category :: Int 
  }


constMinRadius = 2.0
constMaxRadius = 10.0
constMaxDepth = 4
constMinChildren = 2
constMaxChildren = 3

foreign import clientWidth :: forall e. String -> Eff e Number
foreign import clientHeight :: forall e. String -> Eff e Number

data MockData = MockData (Array EFNode) (Array EFLink)

randomInRange :: Number -> Number -> Eff _ Number 
randomInRange min max = do
  x <- random
  pure $ (max - min) * x + min

createRootNode :: forall e. Int -> Eff (random :: RANDOM|e) EFNode
createRootNode depth = do
  width <- clientWidth "force4"
  height <- clientHeight "force4"
  rnd <- random
  rValue <- randomInRange constMinRadius constMaxRadius
  let x = width / 2.0 + (0.5 - rnd) * 200.0
  let y = (height - 20.0) * toNumber depth / (toNumber constMaxDepth + 1.0) + 20.0
  
  pure { name: "ROOT_NODE"
       , value:  rValue
       , id: "root"
       , depth: depth
       , initial: Tuple x y
       , fixX: false
       , fixY: true
       , category: 2
       }

    
createNodeWithIndex :: forall e. String -> Int -> Eff (random :: RANDOM|e) EFNode
createNodeWithIndex idx depth = 
  _{ id = idx
   , name = "NODE_" <> idx
   , category = if depth == constMaxDepth
                then 1
                else 0
   } <$> createRootNode depth
               

mkChild :: forall e. EFNode -> String ->
           Eff (random::RANDOM|e) (Tuple EFNode EFLink)
mkChild node idx = do 
  child <- createNodeWithIndex idx (node.depth + one)
  let link = {source: node.name, target: child.name, weight: 1.0}
  return $ Tuple child link

mkChildren :: forall e. EFNode -> Eff (random::RANDOM|e) (Array (Tuple EFNode EFLink))
mkChildren node = do
  ints <- (0 ..) <$> randomInt constMinChildren constMaxChildren
  for ints \i ->
    mkChild node $ node.id <> ":" <> show i

forceMockThreeDataI :: Array (Tuple EFNode EFLink) -> MockData ->
                       Int -> Eff _ MockData 
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
  name = Just node.name,
  initial = Just node.initial,
  fixX = Just  node.fixX,
  fixY = Just node.fixY,
  category = Just $ toNumber $ node.category
  }


linkNormalize link = Link {
  source: Name link.source,
  target: Name link.target,
  weight: link.weight,
  itemStyle: Nothing
  }

itemColor color = Just $ ItemStyle itemStyleDefault {
  normal = Just $ IStyle istyleDefault {
     color = Just $ SimpleColor color
     }
  }

mkOptions nodes links = Option $ optionDefault {
  series = Just $ Just <$> [
     ForceSeries {
        common: universalSeriesDefault{
           name = Just "Force tree"
           },
        forceSeries: forceSeriesDefault {
          ribbonType = Just true,
          categories = Just [
            ForceCategory $ forceCategoryDefault {
               name = Just "first",
               itemStyle = itemColor "#ff7f50"
               },
            ForceCategory $ forceCategoryDefault {
              name = Just "second",
              itemStyle = itemColor "#6f57bc"
              },
            ForceCategory $ forceCategoryDefault {
              name = Just "third",
              itemStyle = itemColor "#af0000"
              }
            ],
          nodes = Just $ nodeNormalize <$> nodes,
          links = Just $ linkNormalize <$> links,
          minRadius = Just constMinRadius,
          maxRadius = Just constMaxRadius
          }
        
        }
     ]
  }

  


options :: forall e. Eff (random::RANDOM|e) _
options = do
  mockdata <- forceMockThreeData 
  case mockdata of
    MockData nodes links -> do
      return $ mkOptions nodes links

force4 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in force4"
    Just el -> do
      opts <- options
      chart <- init Nothing el >>= setOption opts true
      return unit
      
