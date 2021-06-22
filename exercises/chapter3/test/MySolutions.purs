module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (nubByEq)
import Data.Maybe (Maybe, isNothing)

-- Note to reader: Add your solutions to this file

findEntryByName :: String -> String -> AddressBook -> Maybe Entry
findEntryByName firstName lastName = findEntry filter
  where
  filter :: Entry -> Boolean
  filter = eq lastName <<< _.lastName && eq firstName <<< _.firstName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = findEntry filter
  where
  filter :: Entry -> Boolean
  filter = eq street <<< _.address.street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not isNothing <<< findEntryByName firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubByEq comparator book
  where
    comparator :: Entry -> Entry -> Boolean
    comparator aEntry bEntry = aEntry.firstName == bEntry.firstName && aEntry.lastName == aEntry.lastName