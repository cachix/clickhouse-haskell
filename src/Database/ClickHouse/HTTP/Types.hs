module Database.ClickHouse.HTTP.Types where

import Data.Text (Text)

data HttpConnectionInfo = HttpConnectionInfo
  { secure :: Bool,
    host :: Text,
    port :: Int,
    username :: Text,
    password :: Text,
    database :: Maybe Text,
    queryParams :: [(Text, Text)]
  }

data Command = Ping | Query Text
