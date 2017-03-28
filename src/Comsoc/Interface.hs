module Comsoc.Interface where

import Comsoc.STVA

import Text.Parsec
import Text.Parsec.String

evaluate input = 
    case runP votes () "<input>" input of
        Left err -> print err
        Right vs -> print $ stv vs

type Candidate = [String]

votes :: Parser (Votes Candidate)
votes = do
    vs <- vote `sepBy` newline 
    return $ filter (not . null) vs

vote :: Parser (Vote Candidate)
vote = candidate `sepBy` (char '>' >> blanks) 

candidate :: Parser Candidate
candidate = (many1 $ noneOf "> \n\t")  `endBy1` blanks 

blanks = many (char ' ')
