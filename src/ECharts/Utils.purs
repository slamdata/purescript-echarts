module ECharts.Utils where

import Data.Argonaut.Core

{-
This func is used to construct copy of object, without null and undefined fields.
i.e
{foo: 1, bar: 12, baz: null, quux: undefined} ->
{foo: 1, bar: 12} 
-}

foreign import unnull """
function unnull(obj) {
  if (obj == null
     || typeof(obj) != 'object'
     || obj instanceof window['Date']
     || obj instanceof window['Function']
     || obj instanceof window['RegExp']) {
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



