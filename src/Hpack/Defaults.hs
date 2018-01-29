{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.Defaults (
  ensure
, Defaults(..)
#ifdef TEST
, Result(..)
, ensureFile
#endif
) where

import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.List
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import           System.FilePath
import           System.Directory

import           Hpack.Syntax.Defaults

type URL = String

defaultsUrl :: Defaults -> URL
defaultsUrl Defaults{..} = "https://raw.githubusercontent.com/" ++ defaultsGithubUser ++ "/" ++ defaultsGithubRepo ++ "/" ++ defaultsRef ++ "/" ++ intercalate "/" defaultsPath

defaultsCachePath :: FilePath -> Defaults -> FilePath
defaultsCachePath dir Defaults{..} = joinPath $
  dir : "defaults" : defaultsGithubUser : defaultsGithubRepo : defaultsRef : defaultsPath

data Result = Found | NotFound | Failed String
  deriving (Eq, Show)

get :: URL -> FilePath -> IO Result
get url file = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  case responseStatus response of
    Status 200 _ -> do
      createDirectoryIfMissing True (takeDirectory file)
      LB.writeFile file (responseBody response)
      return Found
    Status 404 _ -> return NotFound
    status -> return (Failed $ "Error while downloading " ++ url ++ " (" ++ formatStatus status ++ ")")

formatStatus :: Status -> String
formatStatus (Status code message) = show code ++ " " ++ B.unpack message

ensure :: FilePath -> Defaults -> IO (Either String FilePath)
ensure dir defaults =
  ensureFile file url >>= \ case
    Found -> return (Right file)
    NotFound -> return (Left notFound)
    Failed err -> return (Left err)
  where
    url = defaultsUrl defaults
    file = defaultsCachePath dir defaults
    notFound = "Invalid value for \"defaults\"! File " ++ url ++ " does not exist!"

ensureFile :: FilePath -> URL -> IO Result
ensureFile file url = do
  doesFileExist file >>= \ case
    True -> return Found
    False -> get url file
