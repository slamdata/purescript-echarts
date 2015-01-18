module Utils where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import DOM
import Math (floor)
import Data.Array
import Data.Maybe

foreign import undefine  """
function undefine(a) {
  return undefined;
}
""" :: forall a. a -> a


foreign import log """
function log(a) {
  return function() {
    console.log(a);
  };
}

""" :: forall a e. a -> Eff e Unit


foreign import getElementById """
function getElementById(id) {
  return function() {
    console.log(id);
    return document.getElementById(id);
  };
}
""" :: forall e. String -> Eff (dom :: DOM|e) Node


foreign import onLoad """
function onLoad(action) {
  return function() {
    window.onload = action;
  };
}
""" :: forall e. Eff e Unit -> Eff e Unit

foreign import windowize """
function windowize(key) {
  return function(a) {
    return function() {
      window[key] = a;
    };
  };
}
""" :: forall e a. String -> a -> Eff e Unit




foreign import precise """
function precise(pre) {
  return function(num) {
    var Math = window['Math'];
    var result = Math.round(Math.pow(10, pre) * num) / Math.pow(10, pre);
    return result;
  };
}
""" :: Number -> Number -> Number

foreign import randomLst """
function randomLst(count) {
  return function() {
    var Math = window['Math'];
    var result = [];
    for (var i = 0; i < count; i++) {
      result.push(Math.random());
    }
    return result;
  };
}
""" :: forall e.  Number -> Eff (random :: Random|e) [Number]


randomInList :: forall a e. [a] -> Eff (random :: Random|e) a
randomInList lst = do 
  let l = length lst
  rnd <- random
  let i = floor (rnd * l)
  return $ case lst !! i of
    Just x -> x
      
