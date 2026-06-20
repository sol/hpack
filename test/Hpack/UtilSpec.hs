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

  describe "tryReadFile" $ do
    it "reads file" $ do
      inTempDirectory $ do
        writeFile "foo" "bar"
        tryReadFile "foo" `shouldReturn` Just "bar"

    it "returns Nothing if file does not exist" $ do
      inTempDirectory $ do
        tryReadFile "foo" `shouldReturn` Nothing

  describe "expandGlobs" $ around withTempDirectory $ do
    it "accepts literal files" $ \dir -> do
      touch (dir </> "foo.js")
      expandGlobs "field-name" dir ["foo.js"] `shouldReturn` ([], ["foo.js"])

    it "keeps declaration order for literal files" $ \dir -> do
      touch (dir </> "foo.js")
      touch (dir </> "bar.js")
      expandGlobs "field-name" dir ["foo.js", "bar.js"] `shouldReturn` ([], ["foo.js", "bar.js"])
    
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
    context "when accepting exclusion patterns" $ do 
      it "removes all files matched" $ \dir -> do 
        let goodfiles = [
                  "files/foo.js"
                , "files/bar.js"
                , "files/baz.js"]
            badfiles = [
                  "files/foo.hs"
                , "files/bar.hs"
                , "files/baz.hs"]
        mapM_ (touch . (dir </>)) goodfiles
        mapM_ (touch . (dir </>)) badfiles
        expandGlobs "field-name" dir ["files/*", "!files/*.hs"] `shouldReturn` ([], sort goodfiles)
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
        expandGlobs "field-name" dir ["*.foo"] `shouldReturn`
          (["Specified pattern \"*.foo\" for field-name does not match any files"], [])

    context "when a pattern only matches a directory" $ do
      it "warns" $ \dir -> do
        createDirectory (dir </> "foo")
        expandGlobs "field-name" dir ["fo?"] `shouldReturn`
          (["Specified pattern \"fo?\" for field-name does not match any files"], [])

    context "when a literal file does not exist" $ do
      it "warns and keeps the file" $ \dir -> do
        expandGlobs "field-name" dir ["foo.js"] `shouldReturn` (["Specified file \"foo.js\" for field-name does not exist"], ["foo.js"])
