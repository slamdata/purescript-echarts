module ECharts.Utils where

import Data.Argonaut.Core

foreign import unnull """
function unnull(obj) {
  if (obj == null || typeof(obj) != 'object') {
    return obj;
  }
  var temp = new obj.constructor();
  for (var i in obj) {
    if (obj.hasOwnProperty(i) && obj[i] !== null && obj[i] !== undefined) {
      temp[i] = unnull(obj[i]);
    }
  }
  return temp;
}
""" :: Json -> Json


foreign import func2json """
function func2json(fn) {
  return fn;
}
""" :: forall a. a -> Json
