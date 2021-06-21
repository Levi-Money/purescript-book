module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry, findEntry)
import Data.Maybe (Maybe, isNothing)

-- Note to reader: Add your solutions to this file

findEntryByName :: String -> String -> AddressBook -> Maybe Entry
findEntryByName firstName lastName = findEntry filter
  where
  filter :: Entry -> Boolean
  filter entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = findEntry filter
  where
  filter :: Entry -> Boolean
  filter = eq street <<< _.address.street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = not isNothing $ findEntryByName firstName lastName book