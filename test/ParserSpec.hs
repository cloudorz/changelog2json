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
      it "cc" $ do 
        parse (changelogName *> changelogDesc *> unreleasedVersion *> versionItem *> versionItem *> diffRecord) 
              "(undefined)"
              "# N\nhh\n=lll\n\n## [Unreleased]\n\n## [1.0.0] - 2007-06-20 [YANKED]\n### Added\n- up-to-date\n\n## [1.0.1] - 2008-06-20\n### Changed\n- hh\n\n[xxx]:adb"
              `shouldBe`
              return (Diff "xxx" "adb")--(Version "1.0.1" "2008-06-20" False [Section "Changed" ["hh"]])
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
      it "should consume all the whitespace after ']'" $ do
        parse (unreleasedVersion *> string "#")
              "(undefined)"
              "## [Unreleased]  \n\n#"
              `shouldBe`
              return "#"
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
    describe "parse the date String" $ do 
      it "parse '2016-06-02'" $ do
        parse dateString
              "(undefined)"
              "2016-06-02"
              `shouldBe`
              return "2016-06-02"
    describe "parse version item" $ do
      it "parse '## [0.0.1] - 2014-08-09\n### Changed\n- change 1\n- change 2\n'" $ do
        parse versionItem
              "(undefined)"
              "## [0.0.1] - 2014-08-09\n### Changed\n- change 1\n- change 2\n"
              `shouldBe`
              return (Version "0.0.1" "2014-08-09" False [Section "Changed" ["change 1", "change 2"] ])
      it "parse '## [0.0.1] - 2014-08-09\n### Changed\n- change 1\n- change 2\n\n### Added  \n- change 3\n'" $ do
        parse versionItem
              "(undefined)"
              "## [0.0.1] - 2014-08-09\n### Changed\n- change 1\n- change 2\n\n### Added  \n- change 3\n"
              `shouldBe`
              return (Version "0.0.1" "2014-08-09" False [Section "Changed" ["change 1", "change 2"], Section "Added" ["change 3"]])

