{-# LANGUAGE TupleSections #-}
module Hpack.Convert.Run
    ( runConvert
    )
  where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob

import           Hpack.Config
import           Hpack.Convert

runConvert :: IO ()
runConvert = do
    as <- getArgs
    (dir, cabalFileFP) <- case as of
        ("--convert":dir:_) -> do
            isFile <- doesFileExist dir
            if takeExtension dir == ".cabal" && isFile
                then return (takeDirectory dir, dir)
                else (dir,) <$> findCabalFileFP dir
        ["--convert"] -> do
            cwd <- getCurrentDirectory
            cabalFileFP <- findCabalFileFP cwd
            return (cwd, cabalFileFP)
        _ -> die "Usage: hpack --convert [ dir | cabal file ]"
    pkg <- runConvert' dir cabalFileFP
    writePackage (dir </> "package.yaml") pkg
    putStrLn $ "generated package.yaml based on " ++ cabalFileFP

findCabalFileFP :: FilePath -> IO FilePath
findCabalFileFP dir = do
    mcabalFileFP <- listToMaybe <$> globDir1 (compile "*.cabal") dir
    case mcabalFileFP of
        Nothing -> die "No cabal file in the current directory"
        Just cabalFileFP -> return cabalFileFP

runConvert' :: FilePath -> FilePath -> IO Package
runConvert' dir cabalFileFP = do
    mpackageYaml <- find (== "package.yaml") <$> getDirectoryContents dir

    when (isJust mpackageYaml) $
        die $ (dir </> "package.yaml") ++ " already exists"

    old <- readFile cabalFileFP
    case fromPackageDescriptionString old of
        Left err -> die (show err)
        Right pkg -> return pkg
