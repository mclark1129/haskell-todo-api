{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap
import Site

main :: IO ()
main = quickHttpServe site