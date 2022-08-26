module Test.MyOverrides.Effect.Alert where

import Effect (Effect)

foreign import confirm :: String -> Effect Boolean
