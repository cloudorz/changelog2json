{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Text (unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Aeson.Encode.Pretty (encodePretty)

data Diff = Diff { tag :: String
                 , link :: String
                 } deriving (Generic, Show, Eq)

data Section = Section { name :: String
                       , entries :: [String]
                       } deriving (Generic, Show, Eq)
                         
data Version = Unreleased | Version { version :: String 
                                    , date :: String
                                    , yanked :: Bool
                                    , sections :: [Section]
                                    } deriving (Generic, Show, Eq)
                         
data Changelog = Changelog { head :: String
                           , desc :: String
                           , versions :: [Version]
                           , diffs :: [Diff]
                           } deriving (Generic, Show, Eq)

-- JSON encode supports
instance ToJSON Diff where
  toJSON (Diff tag link) = object [pack tag .= link]

instance ToJSON Section where
  toJSON (Section name entries) = 
    object ["type" .= name, "changes" .= entries]

instance ToJSON Version where
  toJSON Unreleased = String "Unreleased"
  toJSON (Version version date yanked sections) = 
    object ["version" .= version, "published_at" .= date, "yanked" .= yanked, "groups" .= sections]

instance ToJSON Changelog where
  toJSON (Changelog head desc versions diffs) = 
    object ["framework_name" .= head, "description" .= desc, "versions" .= versions, "diffs" .= diffs]

changelog2json :: Changelog -> String
changelog2json = unpack . decodeUtf8 . toStrict . encode

prettyC2J :: Changelog -> String
prettyC2J = unpack . decodeUtf8 . toStrict . encodePretty

