{-# LANGUAGE LambdaCase #-}
module Hpack.Error (
-- | /__NOTE:__/ This module is exposed to allow integration of Hpack into
-- other tools.  It is not meant for general use by end users.  The following
-- caveats apply:
--
-- * The API is undocumented, consult the source instead.
--
-- * The exposed types and functions primarily serve Hpack's own needs, not
-- that of a public API.  Breaking changes can happen as Hpack evolves.
--
-- As an Hpack user you either want to use the @hpack@ executable or a build
-- tool that supports Hpack (e.g. @stack@ or @cabal2nix@).
  HpackError (..)
, formatHpackError
, ProgramName (..)
, URL
, Status (..)
, formatStatus
) where

import qualified Data.ByteString.Char8 as B
import           Data.List (intercalate)
import           Data.String (IsString (..))
import           Data.Version (Version (..), showVersion)
import           Network.HTTP.Types.Status (Status (..))

type URL = String

data HpackError =
    HpackVersionNotSupported FilePath Version Version
  | DefaultsFileNotFound FilePath
  | DefaultsDownloadFailed URL Status
  | CycleInDefaults [FilePath]
  | ParseError String
  | DecodeValueError FilePath String
  deriving (Eq, Show)

newtype ProgramName = ProgramName {unProgramName :: String}
  deriving (Eq, Show)

instance IsString ProgramName where
  fromString = ProgramName

formatHpackError :: ProgramName -> HpackError -> String
formatHpackError (ProgramName progName) = \ case
  HpackVersionNotSupported file wanted supported ->
    "The file " ++ file ++ " requires version " ++ showVersion wanted ++
    " of the Hpack package specification, however this version of " ++
    progName ++ " only supports versions up to " ++ showVersion supported ++
    ". Upgrading to the latest version of " ++ progName ++ " may resolve this issue."
  DefaultsFileNotFound file -> "Invalid value for \"defaults\"! File " ++ file ++ " does not exist!"
  DefaultsDownloadFailed url status -> "Error while downloading " ++ url ++ " (" ++ formatStatus status ++ ")"
  CycleInDefaults files -> "cycle in defaults (" ++ intercalate " -> " files ++ ")"
  ParseError err -> err
  DecodeValueError file err -> file ++ ": " ++ err

formatStatus :: Status -> String
formatStatus (Status code message) = show code ++ " " ++ B.unpack message
