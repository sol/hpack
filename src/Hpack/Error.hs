{-# LANGUAGE RecordWildCards #-}

-- | Errors returned by Hpack library functions.
module Hpack.Error
  ( HpackError (..)
  , hpackProgName
  , renderHpackError
  , ProgramName (..)
    -- * Re-export of types used in Hpack errors
  , Status (..)
  , Version (..)
  , URL
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.List (intercalate)
import           Data.String (IsString (..))
import           Data.Version (Version (..), showVersion)
import           Network.HTTP.Types.Status (Status (..))

-- | Type synonyn representing URLs.
type URL = String

-- | Type representing errors returned by functions exported by the modules of
-- the Hpack library.
data HpackError
  = HpackVersionUnsupported !FilePath !Version !Version
  | DefaultsFileNotFound !FilePath
  | DefaultsFileUrlNotFound !URL
  | DownloadingFileFailed !URL !Status
  | CycleInDefaultsError ![FilePath]
  | HpackParseAesonException !FilePath !String
  | HpackParseYamlException !FilePath !String
  | HpackParseYamlParseException {
      yamlFile :: !FilePath
    , yamlProblem :: !String
    , yamlContext :: !String
    , yamlIndex :: !Int
      -- ^ Not displayed by 'renderHpackError'.
    , yamlLine :: !Int
    , yamlColumn :: !Int
    }
  | HpackParseOtherException !FilePath !String
  | DecodeValueError !FilePath !String
    -- | Data constructor for users of the Hpack library that do not use the
    -- default 'Hpack.Yaml.decodeYaml' and wish to use 'String' as an
    -- error/exception type.
  | HpackOtherException !FilePath !String
  deriving (Eq, Show)

renderHpackError :: ProgramName -> HpackError -> String
renderHpackError (ProgramName progName) (HpackVersionUnsupported file wanted supported) =
  "The file " ++ file ++ " requires version " ++ showVersion wanted ++
  " of the Hpack package specification, however this version of " ++
  progName ++ " only supports versions up to " ++ showVersion supported ++
  ". Upgrading to the latest version of " ++ progName ++ " may resolve this issue."
renderHpackError _ (DefaultsFileNotFound file) =
  "Invalid value for \"defaults\"! File " ++ file ++ " does not exist!"
renderHpackError _ (DefaultsFileUrlNotFound url) =
  "Invalid value for \"defaults\"! File " ++ url ++ " does not exist!"
renderHpackError _ (DownloadingFileFailed url status) =
  "Error while downloading " ++ url ++ " (" ++ formatStatus status ++ ")"
 where
  formatStatus :: Status -> String
  formatStatus (Status code message) = show code ++ " " ++ B.unpack message
renderHpackError _ (CycleInDefaultsError files) =
  "cycle in defaults (" ++ intercalate " -> " files ++ ")"
renderHpackError _ (HpackParseAesonException file s) = renderFileMsg file s
renderHpackError _ (HpackParseYamlException file s) = renderFileMsg file s
renderHpackError _ (HpackParseYamlParseException{..}) = renderFileMsg yamlFile $
  show yamlLine ++ ":" ++ show yamlColumn ++ ": " ++ yamlProblem ++ " " ++ yamlContext
renderHpackError _ (HpackParseOtherException file s) = renderFileMsg file s
renderHpackError _ (DecodeValueError file s) = renderFileMsg file s
renderHpackError _ (HpackOtherException file s) = renderFileMsg file s

-- | Helper function for renderHpackError
renderFileMsg :: FilePath -> String -> String
renderFileMsg file s = file ++ ": " ++ s

hpackProgName :: ProgramName
hpackProgName = ProgramName "hpack"

-- | Type representing the names of programs using the Hpack library.
newtype ProgramName = ProgramName {unProgramName :: String}
  deriving (Eq, Show)

instance IsString ProgramName where
  fromString = ProgramName
