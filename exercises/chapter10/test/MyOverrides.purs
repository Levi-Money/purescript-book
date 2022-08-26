module Test.MyOverrides where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Control.Alternative (guard)
import Effect (Effect)
import Effect.Storage (getItem)
import Effect.Exception (Error, error)
import Effect.Alert (alert)
import Data.AddressBook (Person, examplePerson)
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import React.Basic.DOM as D
import React.Basic.Hooks as R
import React.Basic.Events (handler_)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM (Element)
import Web.HTML (window)
import Web.HTML.Window (document)
import Main (mkAddressBookApp, processItem)
import Test.MyOverrides.Storage (removeItem)
import Test.MyOverrides.Effect.Alert (confirm)

mkOverrideApp :: Person -> Effect (R.ReactComponent (Record ()))
mkOverrideApp initialPerson = do
  addr <- mkAddressBookApp 
  incReducer <- R.mkReducer (\s _ -> s + 1)
  R.reactComponent "OverrideApp" \_ -> R.do
    (Tuple st inc) <- R.useReducer 0 incReducer
    let resetText = "Are you sure you want to reset the address book?"
        reset true = removeItem "person" *> inc unit
        reset _ = pure unit
        addrEl = R.element addr { initialPerson }
        resetButton = D.label
          { className: "form-group row col-form-label"
          , children: [ D.button { id: "reset"
                                 , className: "btn-secondary btn"
                                 , onClick: handler_ $ confirm resetText >>= reset
                                 , children: [ D.text "Reset" ] } ]
      }
    pure $ D.div { children: [ R.keyed (show st) addrEl, resetButton ] }

getElementById' :: MonadThrow Error Effect => String -> Effect Element
getElementById' id = do
  w <- window
  doc <- document w
  mEl <- getElementById id $ toNonElementParentNode doc
  liftMaybe (error $ "could not find #" <> id) mEl 

main :: Effect Unit
main = do
  ctr <- getElementById' "container"
  item <- getItem "person"
  initialPerson <- case processItem item of
    Left  err -> do
      alert $ "Error: " <> err <> ". Loading examplePerson"
      pure examplePerson
    Right p   -> pure p
  overrideApp <- mkOverrideApp initialPerson
  let app = R.element overrideApp {}
  D.render app ctr
