module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (nubByEq, filter, head)
import Data.Maybe (Maybe, isNothing)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter (eq street <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not isNothing <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubByEq comparator book
  where
    comparator :: Entry -> Entry -> Boolean
    comparator aEntry bEntry = aEntry.firstName == bEntry.firstName && aEntry.lastName == aEntry.lastName
