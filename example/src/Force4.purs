module Force4 where

import Prelude

import Control.Monad. Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (random, randomInt, RANDOM)

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Array (concat, (:), (..))
import Data.Tuple (Tuple(..), fst)
import Data.Traversable (sequence, for, foldl)

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U


type EFLink =
  { source ∷ String
  , target ∷ String
  , weight ∷ Number
  }

type EFNode =
  { value ∷ Number
  , id ∷ String
  , name ∷ String
  , depth ∷ Int
  , initial ∷ Tuple Number Number
  , fixY ∷ Boolean
  , fixX ∷ Boolean
  , category ∷ Int
  }

constMinRadius ∷ Number
constMinRadius = 2.0

constMaxRadius ∷ Number
constMaxRadius = 10.0

constMaxDepth ∷ Int
constMaxDepth = 4

constMinChildren ∷ Int
constMinChildren = 2

constMaxChildren ∷ Int
constMaxChildren = 3

foreign import clientWidth ∷ ∀ e. String → Eff e Number
foreign import clientHeight ∷ ∀ e. String → Eff e Number

data MockData = MockData (Array EFNode) (Array EFLink)

randomInRange ∷ ∀ e. Number → Number → Eff (random ∷ RANDOM |e) Number
randomInRange min max = do
  x ← random
  pure $ (max - min) * x + min

createRootNode ∷ ∀ e. Int → Eff (random ∷ RANDOM|e) EFNode
createRootNode depth = do
  width ← clientWidth "force4"
  height ← clientHeight "force4"
  rnd ← random
  rValue ← randomInRange constMinRadius constMaxRadius
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


createNodeWithIndex ∷ ∀ e. String → Int → Eff (random ∷ RANDOM|e) EFNode
createNodeWithIndex idx depth =
  createRootNode depth
  <#> _{ id = idx
       , name = "NODE_" <> idx
       , category =
           if depth == constMaxDepth
             then 1
             else 0
       }


mkChild
  ∷ ∀ e
  . EFNode
  → String
  → Eff (random ∷ RANDOM|e) (Tuple EFNode EFLink)
mkChild node idx = do
  child ← createNodeWithIndex idx (node.depth + one)
  let link = {source: node.name, target: child.name, weight: 1.0}
  pure $ Tuple child link

mkChildren ∷ ∀ e. EFNode → Eff (random ∷ RANDOM|e) (Array (Tuple EFNode EFLink))
mkChildren node = do
  ints ← (0 .. _) <$> randomInt constMinChildren constMaxChildren
  for ints \i →
    mkChild node $ node.id <> ":" <> show i

forceMockThreeDataI
  ∷ ∀ e
  . Array (Tuple EFNode EFLink)
  → MockData
  → Int
  → Eff (random ∷ RANDOM |e) MockData
forceMockThreeDataI current accum 0 = pure accum
forceMockThreeDataI current accum n = do
  let
    nextAccum =
      foldl (\(MockData nodes links) (Tuple node link) →
              MockData (node:nodes) (link:links)) accum current
  children ← sequence $ mkChildren <<< fst <$> current
  forceMockThreeDataI (concat children) nextAccum (n - 1)

forceMockThreeData ∷ ∀ e. Eff (random ∷ RANDOM|e) MockData
forceMockThreeData = do
  root ← createRootNode 0
  children ← mkChildren root
  forceMockThreeDataI children (MockData [root] []) constMaxDepth

nodes ∷ ∀ e. Eff e (Array EFNode)
nodes = pure []

links ∷ ∀ e. Eff e (Array EFLink)
links = pure []

nodeNormalize ∷ EFNode → E.Node
nodeNormalize node =
  E.Node $ (E.nodeDefault node.value)
    { name = Just node.name
    , initial = Just node.initial
    , fixX = Just  node.fixX
    , fixY = Just node.fixY
    , category = Just $ toNumber $ node.category
    }

linkNormalize ∷ EFLink → E.Link
linkNormalize link =
  E.Link { source: E.Name link.source
         , target: E.Name link.target
         , weight: link.weight
         , itemStyle: Nothing
         }

itemColor ∷ String → Maybe E.ItemStyle
itemColor color =
  Just $ E.ItemStyle E.itemStyleDefault
    { normal = Just $ E.IStyle E.istyleDefault
        { color = Just $ E.SimpleColor color }
    }

mkOptions ∷ Array EFNode → Array EFLink → E.Option
mkOptions nodes' links' =
  E.Option E.optionDefault
    { series = Just $ map Just
        [ E.ForceSeries
            { common: E.universalSeriesDefault
                { name = Just "Force tree" }
            , forceSeries: E.forceSeriesDefault
                { ribbonType = Just true
                , categories = Just
                    [ E.ForceCategory E.forceCategoryDefault
                        { name = Just "first"
                        , itemStyle = itemColor "#ff7f50"
                        }
                    , E.ForceCategory E.forceCategoryDefault
                        { name = Just "second"
                        , itemStyle = itemColor "#6f57bc"
                        }
                    , E.ForceCategory E.forceCategoryDefault
                        { name = Just "third"
                        , itemStyle = itemColor "#af0000"
                        }
                    ]
                , nodes = Just $ map nodeNormalize nodes'
                , links = Just $ map linkNormalize links'
                , minRadius = Just constMinRadius
                , maxRadius = Just constMaxRadius
                }
            }
        ]
    }




options ∷ ∀ e. Eff (random ∷ RANDOM|e) E.Option
options = do
  mockdata ← forceMockThreeData
  case mockdata of
    MockData nodes' links' → do
      pure $ mkOptions nodes' links'

force4
  ∷ ∀ e
  . ElementId
  → Eff (console ∷ CONSOLE, random ∷ RANDOM, dom ∷ DOM, echarts ∷ E.ECHARTS|e) Unit
force4 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in force4"
    Just el → do
      opts ← options
      chart ← E.init Nothing el >>= E.setOption opts true
      pure unit
