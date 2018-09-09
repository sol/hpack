{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Hpack.Render.DependencySpec (spec) where

import           Helper

import           Hpack.Render
import           Hpack.Render.Dsl
import           Hpack.Syntax.Dependencies
import           Hpack.Syntax.DependencyVersion

spec :: Spec
spec = do
  describe "renderDependencies" $ do
    it "renders build-depends" $ do
      let deps =
            [ ("foo", DependencyInfo [] anyVersion)
            ]
      renderDependencies "build-depends" deps `shouldBe`
        [ Field "build-depends" $ CommaSeparatedList
            [ "foo"
            ]
        , Field "mixins" $ CommaSeparatedList []
        ]

    it "renders build-depends with versions" $ do
      let deps =
            [ ("foo", DependencyInfo [] (versionRange ">= 2 && < 3"))
            ]
      renderDependencies "build-depends" deps `shouldBe`
        [ Field "build-depends" $ CommaSeparatedList
            [ "foo >= 2 && < 3"
            ]
        , Field "mixins" $ CommaSeparatedList []
        ]

    it "renders mixins and build-depends for multiple modules" $ do
      let deps =
            [ ("foo", DependencyInfo ["(Foo as Foo1)"] anyVersion)
            , ("bar", DependencyInfo ["hiding (Spam)", "(Spam as Spam1) requires (Mod as Sig)"] anyVersion)
            ]
      renderDependencies "build-depends" deps `shouldBe`
        [ Field "build-depends" $ CommaSeparatedList
           [ "bar"
           , "foo"
           ]
        , Field "mixins" $ CommaSeparatedList
            [ "bar hiding (Spam)"
            , "bar (Spam as Spam1) requires (Mod as Sig)"
            , "foo (Foo as Foo1)"
            ]
        ]
