{-# LANGUAGE FlexibleContexts #-}
module Util where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           System.Directory
import           System.FilePath

import           GHC.Generics
import           Data.Aeson.Types

genericParseJSON_ :: (Generic a, GFromJSON (Rep a)) => String -> Value -> Parser a
genericParseJSON_ name = genericParseJSON defaultOptions {fieldLabelModifier = camelTo '_' . drop (length name)}

newtype List a = List {fromList :: [a]}
  deriving (Eq, Show)

instance FromJSON a => FromJSON (List a) where
  parseJSON v = List <$> (parseJSON v <|> (return <$> parseJSON v))

toModule :: FilePath -> Maybe String
toModule = fmap (map f . reverse) . stripPrefix (reverse ".hs") . reverse
  where
    f c
      | isPathSeparator c = '.'
      | otherwise = c

stripEmptyLines :: String -> String
stripEmptyLines = unlines . reverse . dropWhile null . reverse . dropWhile null . lines

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)
