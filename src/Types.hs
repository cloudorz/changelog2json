module Types where

data Diff = Diff { tag :: String
                 , link :: String
                 } deriving (Show, Eq)

data Section = Section { name :: String
                       , changes :: [String]
                       } deriving (Show, Eq)
                         
data Version = Unreleased | Version { version :: String 
                                    , date :: String
                                    , desc :: String
                                    , sections :: [Section]
                                    } deriving (Show, Eq)
                         
data Changelog = Changelog { head :: String
                           , versions :: [Version]
                           , diffs :: [Diff]
                           } deriving (Show, Eq)

