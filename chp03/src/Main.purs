module Main where

import Prelude

import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, isJust)
import Control.Plus (empty)

type Entry = 
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address = 
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
  entry.firstName <> ": "  <>
  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
  addr.city <> ", " <>
  addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

hasEntry :: String -> String -> AddressBook -> Boolean
hasEntry firstName lastName book = isJust $ findEntry firstName lastName book

findByAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findByAddress street city state = head <<< filter filterAddress 
  where
    filterAddress :: Entry -> Boolean
    filterAddress entry = entry.address.street == street && entry.address.city == city && entry.address.state == state

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameName
  where
    sameName :: Entry -> Entry -> Boolean
    sameName e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
