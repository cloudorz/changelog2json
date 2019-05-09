{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)

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

instance ToJSON Section where

instance ToJSON Version where

instance ToJSON Changelog where

changelog2json :: Changelog -> String
changelog2json = unpack . encode

