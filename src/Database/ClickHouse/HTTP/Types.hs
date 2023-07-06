module Database.ClickHouse.HTTP.Types where

import Data.Text (Text)

data HttpConnectionInfo = HttpConnectionInfo
  { secure :: Bool,
    host :: Text,
    port :: Int,
    username :: Text,
    password :: Text,
    database :: Maybe Text
  }

-- TODO: It's questionable whether this should be part of the API.
data Format = Default | Json

instance Show Format where
  show Default = ""
  show Json = "JSON"

data Command = Ping | Query Format Text
