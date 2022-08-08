module Test.MyOverrides.Storage where 

import Prelude
import Effect (Effect)

foreign import removeItem :: String -> Effect Unit
