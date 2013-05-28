module Main where

import Comsoc.STVA
import Text.Parsec
import Text.Parsec.String

-- | read list of preferences from stdin
-- output a protocol of the vote transferral to stdout
main :: IO ()
main = do
    input <- getContents
    case runP votes () "/dev/stdin" input of
        Left err -> print err
        Right vs -> print $ stv vs
        
type Candidate = [String]

votes :: Parser (Votes Candidate)
votes = vote `sepBy` newline 

vote :: Parser (Vote Candidate)
vote = candidate `sepBy` (char '>' >> spaces) 

candidate :: Parser Candidate
candidate = many1 $ noneOf  "> "  `endBy` spaces 
