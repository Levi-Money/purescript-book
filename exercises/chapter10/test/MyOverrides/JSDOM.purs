module Test.MyOverrides.JSDOM where

import Prelude

import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

foreign import data Dom :: Type
foreign import createJSDOMImpl :: String -> Json -> Effect Dom
foreign import setGlobalWindow :: Dom -> Effect Unit

type DomOptions = { url :: String }

createJSDOM :: String -> DomOptions -> Effect Dom
createJSDOM s o = createJSDOMImpl s $ encodeJson o
