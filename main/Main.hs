module Main where

import Comsoc.Interface (evaluate)

-- | read list of preferences from stdin
-- output a protocol of the vote transferral to stdout
main :: IO ()
main = getContents >>= evaluate
