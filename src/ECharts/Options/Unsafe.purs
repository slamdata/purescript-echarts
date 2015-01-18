module ECharts.Options.Unsafe(
  Merge(..),
  setOptionUnsafe) where
   
import ECharts.Chart
import Data.Maybe
import Data.Function
import Control.Monad.Eff
import Data.Tuple
import Data.Tuple.Nested



foreign import data UntypedContainer :: *

data Merge a = Merge [a]

foreign import mkUntypedContainer """
function mkUntypedContainer() {
  return [];
}
""" :: forall e. Eff e UntypedContainer

foreign import pushToUntypedImpl """
function pushToUntypedImpl(element, container) {
  return function() {
    container.push(element);
    return container;
  };
}
""" :: forall a e. Fn2 a UntypedContainer (Eff e UntypedContainer)

pushToUntyped :: forall a e. a -> UntypedContainer -> Eff e UntypedContainer
pushToUntyped a container = runFn2 pushToUntypedImpl a container
       
foreign import fromTupleImpl """
function fromTupleImpl(nothing, mergeFunc, createFunc, tuple) {
  var result = [];
  var untuplify = function(tuple) {
    
    var val0 = tuple.value0,
        val1 = tuple.value1;
    if (!val0 || !val0.constructor || val0.constructor != nothing.constructor) {
      result.push(val0);
    }
    if (!val1.constructor) {
      result.push(val1);
      return;
    }
    if(val1.constructor == nothing.constructor) {
      return;
    }
    if (val1.constructor.create == createFunc) {
      untuplify(val1);
      return;
    }
    if (val1.constructor.create == mergeFunc) {
      var val2 = val1.value0;
      for (var i = 0; i < val2.length; i++) {
        result.push(val2[i]);
      }
      return;
    }
    result.push(val1);
  };
  untuplify(tuple);
  return result;
}
""" :: forall a b. Fn4
       (Maybe a)
       ([a] -> Merge a)
       (a -> b -> Tuple a b)
       (Tuple a b)
       UntypedContainer

fromTuple :: forall a b. Tuple a b -> UntypedContainer
fromTuple tuple = runFn4 fromTupleImpl Nothing Merge (Tuple) tuple


foreign import setOptionImpl """
function setOptionImpl(option, tupleCreate, fromTuple, merge, echart) {
  return function() {
    var clone = function(obj) {
          if(obj == null || typeof(obj) != 'object')
            return obj;

          var temp = new obj.constructor(); 

          for(var key in obj) {
            if(obj.hasOwnProperty(key)) {
              temp[key] = clone(obj[key]);
            }
          }
          return temp;
        },
        isTuple = function(a) {
          return a && a.constructor && a.constructor.create == tupleCreate;
        },
    

        untuple = function(obj) {
          if (!obj || typeof(obj) != 'object') {
            return obj;
          }

          if (isTuple(obj)) {
            return untuple(fromTuple(obj));
          } 
          for (var key in obj) {
            if(!obj.hasOwnProperty(key)) continue;
            obj[key] = untuple(obj[key]);
          }
          return obj;
        };
    echart.setOption(untuple(clone(option)), merge);
    return echart;
  };
}
""" :: forall options e a b. Fn5
       options 
       (a -> b -> Tuple a b) 
       (Tuple a b -> UntypedContainer) 
       Boolean 
       EChart 
       (Eff e EChart)

setOptionUnsafe :: forall options e. options -> Boolean ->  EChart -> Eff e EChart 
setOptionUnsafe opts merge chart = 
  runFn5 setOptionImpl opts (Tuple) (fromTuple) merge chart 
