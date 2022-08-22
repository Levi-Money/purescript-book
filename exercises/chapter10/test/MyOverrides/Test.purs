module Test.MyOverrides.Test where

import Prelude
import Debug
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.AddressBook (Person, examplePerson)
import Data.Argonaut (encodeJson, stringify)
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Storage (getItem, setItem)
import Effect.Exception (Error, error)
import Effect.Aff (delay)
import Test.Unit (TestSuite, suite, test, testSkip)
import Test.Unit.Assert as Assert
import Test.MyOverrides.JSDOM (createJSDOM, setGlobalWindow)
import Test.MyOverrides.Storage (removeItem)
import Test.MyOverrides (main, getElementById')
import Web.HTML.HTMLElement as HEL 
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLInputElement as HIN
import Web.DOM.Element (toNode)
import Web.DOM.Document (toParentNode)
import Web.DOM.ParentNode (QuerySelector (..), querySelector)
import Web.DOM.Text (fromNode, wholeText)

foreign import isNull :: forall a. a -> Boolean
foreign import act :: Effect Unit -> Effect Unit

clickReset :: MonadThrow Error Effect => Effect Unit
clickReset = do
  el <- getElementById' "reset"
  el' <- liftMaybe (error "could not transform to HTMLElement") $ HEL.fromElement el
  HEL.click el'

runMyOverrides :: TestSuite
runMyOverrides = suite "MyOverrides" do
   suite "Reset - removeItem" do
     test "removes a previously added item" do
        let k = "foo"
            v = "bar"
        d <- liftEffect $ do
           dom <- createJSDOM "" {url:  "http://localhost"}
           setGlobalWindow dom
           setItem k v
           removeItem k
           getItem k
        Assert.assert "value is not null" $ isNull d
   suite "Reset - component - reset" do
      test "removes the person from local storage" do
        d <- liftEffect $ do
           dom <- createJSDOM """<body>
              <div id="container" />
           </body>""" {url:  "http://localhost"}
           setGlobalWindow dom
           setItem "person" $ stringify $ encodeJson examplePerson
           main 
           clickReset
           getItem "person"
        Assert.assert "value is not null" $ isNull d
      testSkip "renders initial person" do
        void $ liftEffect $ do
           dom <- createJSDOM """<body>
              <div id="container" />
           </body>""" {url:  "http://localhost"}
           setGlobalWindow dom
           let p = { firstName: "Foo"
                 , lastName: "Bar"
                 , homeAddress: { street: "A", city: "B", state: "C" }
                 , phones: [] } :: Person
           setItem "person" $ stringify $ encodeJson p
           act main
           act clickReset
           -- it seems that the react component is not being re-rendered with JSDOM, there is not so much we can do here without going deep on react internals.
           -- even tried this without luck:
        -- delay (Milliseconds 1000.0)
        d <- liftEffect $ do
           w <- window
           d <- toDocument <$> document w
           mEl <- querySelector (QuerySelector "input") (toParentNode d)
           el <- liftMaybe (error "could not find input") (HIN.fromElement =<<  mEl)
           HIN.value el
        Assert.equal examplePerson.firstName d
