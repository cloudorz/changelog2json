module Types where

data Diff = Diff { tag :: String
                 , link :: String
                 } deriving (Show, Eq)

data Section = Section { name :: String
                       , entries :: [String]
                       } deriving (Show, Eq)
                         
data Version = Unreleased | Version { version :: String 
                                    , date :: String
                                    , yanked :: Bool
                                    , sections :: [Section]
                                    } deriving (Show, Eq)
                         
data Changelog = Changelog { head :: String
                           , desc :: String
                           , versions :: [Version]
                           , diffs :: [Diff]
                           } deriving (Show, Eq)

