module Test.Main where

import Prelude

import Main (showEntry, showAddress, emptyBook, insertEntry, findEntry, findByAddress, hasEntry, removeDuplicates)

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.List (length)
import Data.Maybe (isJust, isNothing)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "main spec" do
    let address = { street: "123 Fake St.", city: "Faketown", state: "CA" }
    let entry = { firstName: "John", lastName: "Doe", address: address }
    let entryTwo = { firstName: "Indy", lastName: "Jones", address: address }

    it "should show address" do
      showAddress address `shouldEqual` "123 Fake St., Faketown, CA"

    it "should show entry" do
      showEntry entry `shouldEqual` "Doe, John: 123 Fake St., Faketown, CA"

    it "should insert entry" do
      let firstBook = insertEntry entry emptyBook
      length firstBook `shouldEqual` 1

    it "should find entry or nothing" do
      let firstBook = insertEntry entry emptyBook
      let secondBook = insertEntry entryTwo firstBook

      isJust (findEntry "Indy" "Jones" secondBook) `shouldEqual` true
      isNothing (findEntry "" "" emptyBook) `shouldEqual` true

    it "should find entry by address" do
       let firstBook = insertEntry entry emptyBook
       isJust (findByAddress address.street address.city address.state firstBook) `shouldEqual` true
    
    it "should confirm if it contains entry" do
      let firstBook = insertEntry entry emptyBook
      let secondBook = insertEntry entryTwo firstBook

      hasEntry "Indy" "Jones" secondBook `shouldEqual` true

    it "should remove duplicates" do
      let firstBook = insertEntry entry emptyBook
      let secondBook = insertEntry entry firstBook

      length secondBook `shouldEqual` 2
      length (removeDuplicates secondBook) `shouldEqual` 1


