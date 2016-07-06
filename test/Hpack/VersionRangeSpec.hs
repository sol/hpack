module Hpack.VersionRangeSpec where
import Prelude ()
import Prelude.Compat
import Data.Maybe (isNothing, isJust, fromMaybe)
import Data.Version (showVersion)
import Hpack.VersionRange
import Test.Hspec

spec :: Spec
spec = do
  describe "parseVersion" $ do
    context "when version is empty" $
      it "returns Nothing" $ parseVersion "" `shouldBe` Nothing
    context "when version is invalid" $ do
      it "rejects double  dots" $
        parseVersion "1..2.3" `shouldBe` Nothing
      it "rejects invalid characters" $
        parseVersion "1.a" `shouldBe` Nothing
      it "rejects negative versions" $
        parseVersion "-2" `shouldBe` Nothing
    context "when version is valid" $ do
      it "accepts a single integer" $
        showVersion <$> (parseVersion "42") `shouldBe` Just "42"
      it "accepts multi-component versions" $
        showVersion <$> (parseVersion "3.14") `shouldBe` Just "3.14"
      it "accepts 3.9.0" $
        showVersion <$> (parseVersion "3.9.0") `shouldBe` Just "3.9.0"
  describe "Version ordering" $ do
    it "Orders on major version (1)" $
      parseVersion "2.1" > parseVersion "1.5"
    it "Orders on major version (2)" $
      parseVersion "2.2" < parseVersion "2.4"
    it "Can compare versions of differing lengths" $
      parseVersion "2.3" > parseVersion "2.2.5"
  describe "parseVersionRange" $ do
    it "Rejects invalid ranges: ><" $
      isNothing (parseVersionRange ">< 2")
    it "Rejects invalid ranges: 3 <" $
      isNothing (parseVersionRange "3 <")
    it "Rejects invalid ranges: 3 < 4" $
      isNothing (parseVersionRange "3 < 4")
    it "Rejects unbalanced opening parentheses" $
      isNothing (parseVersionRange "(< 5.2")
    it "Rejects unbalanced closing parentheses" $
      isNothing (parseVersionRange "< 5.2)")
    let pv x = fromMaybe (error $ "Assumed we could parse "++x)
                         (parseVersion x)
        lexedDisjunction = lexVersionRange "< 2 || > 5.2"
        lexedConjunction = lexVersionRange "(> 3 && < 4) || (> 6 && < 6.2)"
    context "Lexing" $ do
      it "Lexes disjunction" $
        lexedDisjunction `shouldBe`
          Just [LexLt, LexVersion (pv "2"), LexOr, LexGt, LexVersion (pv "5.2")]
      it "Lexes conjunction" $
         lexedConjunction `shouldBe`
           Just [ LexLParen, LexGt, LexVersion (pv "3"), LexAnd, LexLt
                , LexVersion (pv "4"), LexRParen, LexOr, LexLParen, LexGt
                , LexVersion (pv "6"), LexAnd, LexLt, LexVersion (pv "6.2")
                , LexRParen ]
    context "Parsing" $ do
      it "Parses disjunction" $
        (lexedDisjunction >>= parseVersionCmp) `shouldBe`
          Just (VOr (VLt (pv "2")) (VGt (pv "5.2")))
      it "Parses conjunction" $
        (lexedConjunction >>= parseVersionCmp) `shouldBe`
          Just (VOr (VAnd (VGt (pv "3")) (VLt (pv "4")))
                    (VAnd (VGt (pv "6")) (VLt (pv "6.2"))))
    context "Less than constraint" $ do
      let Just v = parseVersionRange "< 2"
      it "Accepts less than" $ (v <$> parseVersion "1.9") `shouldBe` Just True
      it "Rejects equal" $ (v <$> parseVersion "2") `shouldBe` Just False
      it "Rejects greater" $ (v <$> parseVersion "2.1") `shouldBe` Just False
    context "Less-than-or-equal" $ do
      let Just v = parseVersionRange "<= 2.1"
      it "Accepts less than" $ (v <$> parseVersion "2") `shouldBe` Just True
      it "Accepts equal" $ (v <$> parseVersion "2.1") `shouldBe` Just True
      it "Rejects greater" $ (v <$> parseVersion "2.1.1") `shouldBe` Just False
    context "Disjunction of ranges" $ do
      let v'@(~(Just v)) = parseVersionRange "< 2 || > 5.2"
      it "Parses disjunctions" $ isJust v'
      it "Accepts left disjunct" $
        v <$> parseVersion "1.5" `shouldBe` Just True
      it "Accepts right disjunct" $
        v <$> parseVersion "5.2.1" `shouldBe` Just True
      it "Rejects appropriately" $
        (v <$> parseVersion "3") `shouldBe` Just False
    context "Conjunction of ranges" $ do
      let v'@ (~ (Just v)) = parseVersionRange "> 3 && < 4"
      it "Parses conjunction" $ isJust v'
      it "Accepts appropriately" $
        v <$> parseVersion "3.9.0" `shouldBe` Just True
      it "Rejects appropriately" $
        v <$> parseVersion "4.0.1" `shouldBe` Just False
    context "Conjunction of disjunctions" $ do
      let v'@(~(Just v)) = parseVersionRange "(> 3 && < 4) || (> 6 && < 6.2)"
      it "Parses" (isJust v')
      it "Rejects" (v <$> parseVersion "5" `shouldBe` Just False)
      it "Accepts on the left" $
        (v <$> parseVersion "3.9.0") `shouldBe` Just True
      it "Accepts on the right" $
        (v <$> parseVersion "6.0.1") `shouldBe` Just True
