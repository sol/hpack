{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           System.Environment

import qualified Hpack
import           Hpack.Config
import           Control.Exception

main :: IO ()
main = getArgs >>= \ case
  ["list"] -> exposedModules packageConfig >>= mapM_ (putStrLn . unModule)
  args -> Hpack.getOptions packageConfig args >>= mapM_ (uncurry Hpack.hpack)

exposedModules :: FilePath -> IO [Module]
exposedModules file = readPackageConfig defaultDecodeOptions {decodeOptionsTarget = file} >>= \ case
  Left err -> throwIO $ ErrorCall err
  Right result -> return $ modules result
  where
    modules :: DecodeResult -> [Module]
    modules = maybe [] (libraryExposedModules . sectionData) . packageLibrary . decodeResultPackage
