module Path where

import qualified Data.List as List
import           System.FilePath
import           Data.String

fromFilePath :: FilePath -> Path
fromFilePath = Path . map PathComponent . splitDirectories

toFilePath :: Path -> FilePath
toFilePath = joinPath . components

components :: Path -> [String]
components = map unPathComponent . unPath

newtype Path = Path {unPath :: [PathComponent]}
  deriving Eq

instance Show Path where
  show = show . toFilePath

instance IsString Path where
  fromString = fromFilePath

newtype PathComponent = PathComponent {unPathComponent :: String}
  deriving Eq

stripPrefix :: Path -> Path -> Maybe Path
stripPrefix (Path xs) (Path ys) = Path <$> List.stripPrefix xs ys
