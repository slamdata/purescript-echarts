module ECharts.Connect (
  connect, Connection()
  ) where
import Prelude
import Data.Function
import ECharts.Chart
import Control.Monad.Eff
import ECharts.Effects

newtype Connection = Connection (forall eff. Eff (disconnect::DISCONNECT|eff) Unit)

foreign import connectImpl :: forall e. Fn2 EChart EChart (Eff (connect::CONNECT|e) Connection)

connect :: forall e. EChart -> EChart -> Eff (connect::CONNECT|e) Connection
connect target source = do
  runFn2 connectImpl target source


