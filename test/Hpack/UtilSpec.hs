module Hpack.UtilSpec (main, spec) where

import           Helper
import           System.Directory

import           Hpack.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sort" $ do
    it "sorts lexicographically" $ do
      sort ["foo", "Foo"] `shouldBe` ["Foo", "foo" :: String]

  describe "parseMain" $ do
    it "accepts source file" $ do
      parseMain "Main.hs" `shouldBe` ("Main.hs", [])

    it "accepts literate source file" $ do
      parseMain "Main.lhs" `shouldBe` ("Main.lhs", [])

    it "accepts module" $ do
      parseMain "Foo" `shouldBe` ("Foo.hs", ["-main-is Foo"])

    it "accepts hierarchical module" $ do
      parseMain "Foo.Bar.Baz" `shouldBe` ("Foo/Bar/Baz.hs", ["-main-is Foo.Bar.Baz"])

    it "accepts qualified identifier" $ do
      parseMain "Foo.bar" `shouldBe` ("Foo.hs", ["-main-is Foo.bar"])

  describe "toModule" $ do
    it "maps .hs paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.hs"]  `shouldBe` Just "Foo.Bar.Baz"

    it "maps .lhs paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.lhs"] `shouldBe` Just "Foo.Bar.Baz"

    it "maps .hsc paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.hsc"] `shouldBe` Just "Foo.Bar.Baz"

    it "rejects invalid module names" $ do
      toModule ["resources", "hello.hs"] `shouldBe` Nothing

  describe "getModuleFilesRecursive" $ do
    it "gets all files from given directory" $ do
      inTempDirectory $ do
        touch "foo/bar"
        touch "foo/baz"
        actual <- getModuleFilesRecursive "foo"
        actual `shouldMatchList` [
            ["bar"]
          , ["baz"]
          ]

    it "descends into subdirectories" $ do
      inTempDirectory $ do
        touch "foo/Bar/baz"
        getModuleFilesRecursive "foo" `shouldReturn` [["Bar", "baz"]]

    context "when a subdirectory is not a valid module name" $ do
      it "does not descend" $ do
        inTempDirectory $ do
          touch "foo/bar/baz"
          getModuleFilesRecursive "foo" `shouldReturn` empty

  describe "tryReadFile" $ do
    it "reads file" $ do
      inTempDirectory $ do
        writeFile "foo" "bar"
        tryReadFile "foo" `shouldReturn` Just "bar"

    it "returns Nothing if file does not exist" $ do
      inTempDirectory $ do
        tryReadFile "foo" `shouldReturn` Nothing

  describe "expandGlobs" $ around withTempDirectory $ do
    it "accepts simple files" $ \dir -> do
        touch (dir </> "foo.js")
        expandGlobs "field-name" dir ["foo.js"] `shouldReturn` ([], ["foo.js"])

    it "removes duplicates" $ \dir -> do
      touch (dir </> "foo.js")
      expandGlobs "field-name" dir ["foo.js", "*.js"] `shouldReturn` ([], ["foo.js"])

    it "rejects directories" $ \dir -> do
      touch (dir </> "foo")
      createDirectory (dir </> "bar")
      expandGlobs "field-name" dir ["*"] `shouldReturn` ([], ["foo"])

    it "rejects character ranges" $ \dir -> do
      touch (dir </> "foo1")
      touch (dir </> "foo2")
      touch (dir </> "foo[1,2]")
      expandGlobs "field-name" dir ["foo[1,2]"] `shouldReturn` ([], ["foo[1,2]"])

    context "when expanding *" $ do
      it "expands by extension" $ \dir -> do
        let files = [
                "files/foo.js"
              , "files/bar.js"
              , "files/baz.js"]
        mapM_ (touch . (dir </>)) files
        touch (dir </> "files/foo.hs")
        expandGlobs "field-name" dir ["files/*.js"] `shouldReturn` ([], sort files)

      it "rejects dot-files" $ \dir -> do
        touch (dir </> "foo/bar")
        touch (dir </> "foo/.baz")
        expandGlobs "field-name" dir ["foo/*"] `shouldReturn` ([], ["foo/bar"])

      it "accepts dot-files when explicitly asked to" $ \dir -> do
        touch (dir </> "foo/bar")
        touch (dir </> "foo/.baz")
        expandGlobs "field-name" dir ["foo/.*"] `shouldReturn` ([], ["foo/.baz"])

      it "matches at most one directory component" $ \dir -> do
        touch (dir </> "foo/bar/baz.js")
        touch (dir </> "foo/bar.js")
        expandGlobs "field-name" dir ["*/*.js"] `shouldReturn` ([], ["foo/bar.js"])

    context "when expanding **" $ do
      it "matches arbitrary many directory components" $ \dir -> do
        let file = "foo/bar/baz.js"
        touch (dir </> file)
        expandGlobs "field-name" dir ["**/*.js"] `shouldReturn` ([], [file])

    context "when a pattern does not match anything" $ do
      it "warns" $ \dir -> do
        expandGlobs "field-name" dir ["foo"] `shouldReturn`
          (["Specified pattern \"foo\" for field-name does not match any files"], [])

    context "when a pattern only matches a directory" $ do
      it "warns" $ \dir -> do
        createDirectory (dir </> "foo")
        expandGlobs "field-name" dir ["foo"] `shouldReturn`
          (["Specified pattern \"foo\" for field-name does not match any files"], [])
