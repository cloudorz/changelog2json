module Changelog2json
    ( changelogContent2json
    ) where

import Parser
import Types
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Bifunctor (first)
import Data.Either (either)


type ChangelogError = String

parseChangelog :: String -> Either ChangelogError Changelog
parseChangelog = first errorBundlePretty <$> parse changelogParser "(undefined)"

changelogContent2json :: String -> String
changelogContent2json = either id changelog2json . parseChangelog

