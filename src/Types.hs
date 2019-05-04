module Types where

import Data.Time.Clock (UTCTime)

data Section = Section 
  { name :: String
  , changes :: [String]
  } deriving (Show)
                         
data Version = Version 
  { version :: String
  , date :: UTCTime
  , desc :: String
  , sections :: [Section]
  } deriving (Show)
                         
data Changelog = Changelog 
  { head :: String
  , versions :: [Version]
  } deriving (Show)

