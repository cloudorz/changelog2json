module Main where

import Changelog2json

main :: IO ()
main = interact changelogContent2json

