{-# LANGUAGE OverloadedStrings #-}

module Houndell.Network.HTTP (HoundSettings(..), HoundSearchParams(..), fetchResults, toRequest) where

import           Control.Monad.Catch
import           Data.Aeson                (FromJSON (..), Value (..), decode,
                                            eitherDecode, (.:))
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as LS8
import           Data.Text.Lazy            (Text (..))
import           Houndell.Models
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)

data HoundSettings = HoundSettings {
    url   :: String,
    params :: HoundSearchParams
}

data HoundSearchParams = HoundSearchParams {
    searchQuery :: String,
    searchRepos :: Maybe String,
    searchFiles :: Maybe String,
    searchIgnoreCase :: Bool
}

houndSearchEndpoint = "/api/v1/search"

setHoundParams :: HoundSettings -> Request -> Request
setHoundParams s = setQueryString [
    ("stats", Just "fosho"),
    ("repos", Just $ getSearchRepos s),
    ("rng", Just ":20"),
    ("q", Just $ getSearchQuery s),
    ("files", maybeGetSearchFiles s),
    ("i", Just $ getIgnoreCase s)
  ]

getSearchQuery :: HoundSettings -> S8.ByteString
getSearchQuery s = S8.pack (searchQuery $ params s)

getSearchRepos :: HoundSettings -> S8.ByteString
getSearchRepos s = case searchRepos (params s) of
    Just r -> S8.pack r
    Nothing -> "*"

maybeGetSearchFiles :: HoundSettings -> Maybe S8.ByteString
maybeGetSearchFiles s = S8.pack <$> searchFiles (params s)

getIgnoreCase :: HoundSettings -> S8.ByteString
getIgnoreCase s = if searchIgnoreCase (params s) then "fosho" else "nope"

toRequest :: MonadThrow m => HoundSettings -> m Request
toRequest s = setHoundParams s <$> parseUrl (url s ++ houndSearchEndpoint)

mkRequest :: HoundSettings -> IO (Response LS8.ByteString)
mkRequest s = do
    manager <- newManager defaultManagerSettings
    request <- toRequest s
    httpLbs request manager

parseRequest :: IO (Response LS8.ByteString) -> IO (Maybe HoundResponse)
parseRequest r = decode.responseBody <$> r

fetchResults :: HoundSettings -> IO (Maybe HoundResponse)
fetchResults = parseRequest.mkRequest
