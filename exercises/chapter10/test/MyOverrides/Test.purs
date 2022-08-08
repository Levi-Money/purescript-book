module Test.MyOverrides.Test where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Either (Either (..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Effect.Storage (getItem, setItem)
import Test.MyOverrides.JSDOM (createJSDOM, setGlobalWindow)

foreign import isNull :: forall a. a -> Boolean

runMyOverrides :: TestSuite
runMyOverrides = suite "MyOverrides - removeItem" do
     test "removes a previously added item" do
        let k = "foo"
            v = "bar"
        d <- liftEffect $ do
           dom <- createJSDOM {url:  "http://localhost"}
           setGlobalWindow dom
           setItem k v
           getItem k
        Assert.assert "value is not null" $ isNull d
