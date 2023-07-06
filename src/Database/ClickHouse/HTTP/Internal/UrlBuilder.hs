module Database.ClickHouse.HTTP.Internal.UrlBuilder where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Database.ClickHouse.HTTP.Types
import Network.URI.Encode qualified as Network.Encode

-- URL builders

newUrl :: HttpConnectionInfo -> Command -> Text
newUrl conn cmd =
  let databaseParam = queryParamFromMaybe "database" conn.database
      (path, queryParams) = buildCommand cmd
   in Text.Lazy.toStrict $
        Builder.toLazyText $
          mconcat
            [ Builder.fromText (if conn.secure then "https://" else "http://"),
              Builder.fromText conn.host,
              Builder.singleton ':',
              Builder.fromString (show conn.port),
              Builder.singleton '/',
              Builder.fromText path,
              if null queryParams
                then mempty
                else
                  Builder.singleton '?'
                    <> renderQueryParams (buildQueryParams (databaseParam : conn.queryParams ++ queryParams))
            ]

type QueryParam = (Text, Text)

buildCommand :: Command -> (Text, [QueryParam])
buildCommand cmd =
  case cmd of
    Ping ->
      ("ping", [])
    Query queryText ->
      ("", [("query", Network.Encode.encodeText queryText)])

queryParamFromMaybe :: Text -> Maybe Text -> QueryParam
queryParamFromMaybe key = maybe mempty (key,)

buildQueryParams :: [QueryParam] -> [QueryParam]
buildQueryParams = filter (not . Text.null . fst)

renderQueryParams :: [QueryParam] -> Builder.Builder
renderQueryParams =
  mconcat . intersperse (Builder.singleton '&') . fmap renderQueryParam

renderQueryParam :: QueryParam -> Builder.Builder
renderQueryParam (key, value) =
  Builder.fromText key <> Builder.singleton '=' <> Builder.fromText value
