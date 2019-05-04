module ParserSpec where

import Test.Hspec
import Parser
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Changelog Parser" $ do 
    describe "parse changelog Name/Title " $ do 
      it "parse \"# hello, 你好\n\"" $ do
        parse parseChangelogName "(undefined)" "# hello, 你好\n" `shouldBe` return "hello, 你好"

