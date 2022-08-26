module Test.MyOverrides.Test where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.AddressBook (Person, examplePerson)
import Data.Argonaut (encodeJson, stringify)
import Data.Tuple (Tuple (..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Storage (getItem, setItem)
import Effect.Exception (Error, error)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Ref (new, write, read)
import Test.Unit (TestSuite, suite, test, testSkip, timeout)
import Test.Unit.Assert as Assert
import Test.MyOverrides.JSDOM (createJSDOM, setGlobalWindow)
import Test.MyOverrides.Storage (removeItem)
import Test.MyOverrides.Effect.Alert (confirm)
import Test.MyOverrides (main, getElementById')
import Web.HTML.HTMLElement as HEL 
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLInputElement as HIN
import Web.DOM.Document (toParentNode)
import Web.DOM.ParentNode (QuerySelector (..), querySelector)

foreign import isNull :: forall a. a -> Boolean
foreign import act :: Effect Unit -> Effect Unit
foreign import mockWindowInstanceProperty :: forall a. String -> a -> Effect Unit

clickReset :: MonadThrow Error Effect => Effect Unit
clickReset = do
  el <- getElementById' "reset"
  el' <- liftMaybe (error "could not transform to HTMLElement") $ HEL.fromElement el
  HEL.click el'

data Spy a = NoSpy | Spy (a -> Effect Unit)

fakeConfirmImpl :: Boolean -> Spy String -> String -> Boolean
fakeConfirmImpl r (Spy h) s = unsafePerformEffect $ h s *> pure r
fakeConfirmImpl r (NoSpy) _ = r

mockWinInstConfirm :: Boolean -> Spy String -> Effect Unit
mockWinInstConfirm r h = mockWindowInstanceProperty "confirm" $ fakeConfirmImpl r h

runMyOverrides :: TestSuite
runMyOverrides = suite "MyOverrides" do
   suite "Effect.Alert (confirm)" do
     test "calls Window.confirm with a message and returns its return" $ do
        let ret = true
            msg = "foobar"
        Tuple msg' ret' <- liftEffect $ do
           ref <- new mempty
           dom <- createJSDOM "" {url:  "http://localhost"}
           setGlobalWindow dom 
           mockWinInstConfirm ret $ Spy $ (flip write) ref
           r <- confirm msg
           m <- read ref
           pure $ Tuple m r
        Assert.equal msg msg'
        Assert.equal ret ret'
   suite "Storage (removeItem)" do
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
      test "asks for user confirmation" $ timeout 3000 $ do
        msg <- liftEffect $ do
           dom <- createJSDOM """<body>
             <div id="container" />
           </body>""" {url:  "http://localhost"}
           setGlobalWindow dom
           ref <- new mempty
           mockWinInstConfirm true $ Spy $ (flip write) ref
           main
           clickReset
           read ref
        Assert.equal "Are you sure you want to reset the address book?" msg
      test "removes the person from local storage" do
        d <- liftEffect $ do
           dom <- createJSDOM """<body>
              <div id="container" />
           </body>""" {url:  "http://localhost"}
           setGlobalWindow dom
           mockWinInstConfirm true $ NoSpy
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

