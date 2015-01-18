module ECharts.Connect (
  connect, Connection(), Connect(), Disconnect()
  ) where


import Data.Function
import ECharts.Chart
import Control.Monad.Eff

foreign import data Connect :: !
foreign import data Disconnect :: !

newtype Connection = Connection (forall eff. Eff (disconnect::Disconnect|eff) Unit)

foreign import connectImpl """
function connectImpl(target, source) {
  return function() {
    source.connect(target);
    return function() {
      source.disconnect(target);
    };
  };
}
""" :: forall e. Fn2 EChart EChart (Eff (connect::Connect|e) Connection)

connect :: forall e. EChart -> EChart -> Eff (connect::Connect|e) Connection
connect target source = do
  runFn2 connectImpl target source
    

