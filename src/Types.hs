module Types where

import Data.Time.Clock (UTCTime)

data Section = Section 
  { name :: String
  , changes :: [String]
  } deriving (Show, Eq)
                         
data Version = Unreleased | Version 
  { version :: String
  , date :: UTCTime
  , desc :: String
  , sections :: [Section]
  } deriving (Show, Eq)
                         
data Changelog = Changelog 
  { head :: String
  , versions :: [Version]
  } deriving (Show, Eq)

