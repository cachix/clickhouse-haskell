{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Data.Aeson
import Database.ClickHouse.HTTP qualified as CH
import GHC.Generics
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ping" $ do
    it "pings" $ do
      connection <- CH.connect CH.defaultConnectionInfo
      resp <- CH.ping connection
      resp `shouldBe` "Ok.\n"

  describe "query" $ do
    it "queries" $ do
      connection <- CH.connect CH.defaultConnectionInfo
      resp <- CH.query "SELECT 1" connection
      resp `shouldBe` "1\n"

    it "queries JSON" $ do
      connection <- CH.connect CH.defaultConnectionInfo
      resp <- CH.queryJson "SELECT 1 as x" connection
      resp `shouldBe` Right [JSONResponse {x = 1}]

data JSONResponse = JSONResponse {x :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)
