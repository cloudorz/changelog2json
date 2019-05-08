module ParserSpec where

import Test.Hspec
import Parser
import Types
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do
  describe "Changelog Parser" $ do 
    describe "parse changelog Name/Title " $ do 
      it "parse \"# hello, 你好\n\"" $ do
        parse changelogName 
              "(undefined)" "# hello, 你好\n" 
              `shouldBe` 
              return "hello, 你好"
      it "consume space followed by" $ do
        parse (changelogName *> char 'k') 
              "(undefined)" "# hello, 你好\n   k"
              `shouldBe` 
              return 'k'
    describe "parse changelog description " $ do 
      it "parse 'All notable changes is based \n\n second line \n 第三行 ## '" $ do 
        parse changelogDesc 
              "(undefined)" 
              "All notable changes is based \n\n second line \n 第三行 ## " 
              `shouldBe` 
              return "All notable changes is based \n\n second line \n 第三行 "
      it "not consume after ## " $ do 
        parse (changelogDesc *> string "## ") 
              "(undefined)" 
              "All 哈 \n ## "
              `shouldBe`
              return "## "
    describe "parse Unreleased version" $ do
      it "parse '## [Unreleased]'" $ do
        parse unreleasedVersion 
              "(undefined)"
              "## [Unreleased]  "
              `shouldBe`
              return Unreleased
    describe "parse diff link entry" $ do
      it "parse [1.x.x]: abc568" $ do
        parse diffRecord
              "(undefined)"
              "[1.x.x]: abc568"
              `shouldBe`
              return (Diff "1.x.x" "abc568")
    describe "parse changes entries" $ do
      it "parse -   我是 content\n" $ do 
        parse changesEntry
              "(undefined)"
              "-   我是 content\n"
              `shouldBe`
              return "我是 content"
      it "parse -   我是 content" $ do 
        parse changesEntry
              "(undefined)"
              "-   我是 content"
              `shouldBe`
              return "我是 content"
    describe "parse section name" $ do
      it "parse '### Added" $ do
        parse sectionName
              "(undefined)"
              "### Added"
              `shouldBe`
              return "Added"
      it "parse '### Added\n" $ do
        parse sectionName
              "(undefined)"
              "### Added\n"
              `shouldBe`
              return "Added"
    describe "parse the whole section" $ do
      it "parse '### Added\n- Better lives\n- better work'" $ do 
        parse section
              "(undefined)"
              "### Added\n- Better lives\n- better work"
              `shouldBe`
              return (Section "Added" ["Better lives", "better work"])

