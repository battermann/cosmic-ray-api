{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main,
  )
where

import Connect4.Board
import Connect4.Events
import Connect4.Types
import Data.Aeson
import Data.UUID
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

{- HLINT ignore "Redundant do" -}

main :: IO ()
main = hspec $ do
  describe "Board" $ do
    it "all columns should be legal on empty board" $ do
      legalMoves empty `shouldBe` [0 .. 6]
    it "full column not playable" $ do
      let b = foldl (flip play) empty (Column <$> [4, 4, 4, 4, 4, 4])
      legalMoves b `shouldBe` [0, 1, 2, 3, 5, 6]
  describe "JSON" $ do
    it "serialize/de-serialize" $ do
      let expected = Just (GameCreated (StreamId nil) (ClientId nil) Yellow)
      let actual = decode (encode $ GameCreated (StreamId nil) (ClientId nil) Yellow)
      actual `shouldBe` expected
