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

import           Imports

import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as LB
import           System.FilePath
import           System.Directory

import           Hpack.Error (HpackError (..))
import           Hpack.Syntax.Defaults

type URL = String

defaultsUrl :: Github -> URL
defaultsUrl Github{..} = "https://raw.githubusercontent.com/" ++ githubOwner ++ "/" ++ githubRepo ++ "/" ++ githubRef ++ "/" ++ intercalate "/" githubPath

defaultsCachePath :: FilePath -> Github -> FilePath
defaultsCachePath dir Github{..} = joinPath $
  dir : "defaults" : githubOwner : githubRepo : githubRef : githubPath

data Result = Found | NotFound | Failed URL Status
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
    status -> return (Failed url status)

ensure :: FilePath -> FilePath -> Defaults -> IO (Either HpackError FilePath)
ensure userDataDir dir = \ case
  DefaultsGithub defaults -> do
    let
      url = defaultsUrl defaults
      file = defaultsCachePath userDataDir defaults
    ensureFile file url >>= \ case
      Found -> return (Right file)
      NotFound -> return (Left $ DefaultsFileUrlNotFound url)
      Failed url' status -> return (Left $ DownloadingFileFailed url' status)
  DefaultsLocal (Local ((dir </>) -> file)) -> do
    doesFileExist file >>= \ case
      True -> return (Right file)
      False -> return (Left $ DefaultsFileNotFound file)

ensureFile :: FilePath -> URL -> IO Result
ensureFile file url = do
  doesFileExist file >>= \ case
    True -> return Found
    False -> get url file
