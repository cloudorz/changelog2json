module Changelog2json
    ( someFunc
    ) where

import Parser
import Types
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Bifunctor (first)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type ChangelogError = String

parseChangelog :: String -> Either ChangelogError Changelog
parseChangelog = first errorBundlePretty <$> parse changelogParser "(undefined)"

