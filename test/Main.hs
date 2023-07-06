{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Data.Aeson
import Database.ClickHouse.HTTP qualified as CH
import Database.ClickHouse.HTTP.Internal.UrlBuilder qualified as UrlBuilder
import Database.ClickHouse.HTTP.Types
import GHC.Generics
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "url" $ do
    it "builds a url for the default connection" $
      UrlBuilder.newUrl CH.defaultConnectionInfo (Query "SELECT 1")
        `shouldBe` "http://localhost:8123/?query=SELECT%201"

    it "builds a url for a custom connection" $
      let connectionInfo =
            CH.defaultConnectionInfo
              { secure = True,
                host = "example",
                port = 54000,
                CH.database = Just "test"
              }
       in UrlBuilder.newUrl connectionInfo (Query "SELECT 1")
            `shouldBe` "https://example:54000/?database=test&query=SELECT%201"

    it "passes through the default query params" $
      let connectionInfo =
            CH.defaultConnectionInfo
              { CH.queryParams = [("date_time_output_format", "iso")]
              }
       in UrlBuilder.newUrl connectionInfo (Query "SELECT 1")
            `shouldBe` "http://localhost:8123/?date_time_output_format=iso&query=SELECT%201"

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
      resp <- CH.queryJson "SELECT 1 as x FORMAT JSON" connection
      resp `shouldBe` Right [JSONResponse {x = 1}]

data JSONResponse = JSONResponse {x :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)
