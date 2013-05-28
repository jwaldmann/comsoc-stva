-- | single transferrable vote algorithm,
-- with tie break resulution as given here:
-- http://www.cs.miami.edu/~geoff/Conferences/CADE//Bylaws.html#STVAlgorihm

module Comsoc.STVA where

import qualified Data.Map as M
import Control.Monad ( guard )
import Data.List ( maximumBy, minimumBy, sortBy )
import Data.Ord ( comparing )

import Text.PrettyPrint.HughesPJ

-- | preference is decreasing (head of vote is preference 1, etc.)
type Vote a = [ a ]
type Votes a = [ Vote a ]

-- | describe election of one position
stv :: ( Show a, Ord a ) => Votes a -> Doc
stv vs = text ( "votes: " ++ show vs ) $$
    case major vs of
        Just top -> 
            text $ "majority: " ++ show top 
        Nothing ->    
            let tab = table vs
                ws   = weakest tab
            in  vcat 
                [ text ( "positions: " ++ show tab )    
                , text ( "weakest: " ++ show ws )
                , if ( length ws > 1 ) 
                  then text ( "tied for weakest, explore all options")
                  else empty
                , vcat $ do
                     w <- ws
                     return $ text ( "eliminate: " ++ show w )
                            $$ nest 4 ( stv ( remove w vs ) )
                ]  
                
-- | the candidate with a majority (= more than half)
-- of first preferences, or Nothing.
major :: Ord a => Votes a -> Maybe a
major vs = do
    let m = M.fromListWith (+) 
          $ zip ( map head vs ) ( repeat 1 )
    let ( top, count ) 
          = maximumBy ( comparing snd ) 
          $ M.toList m
    guard $ 2 * count > length vs         
    return top

-- | remove votes for this candidate
remove :: Eq a => a -> Votes a -> Votes a
remove c = map ( filter ( /= c ))

-- | weakest candidates (several, in case of tie)
weakest :: M.Map a [Integer] -> [a]
weakest tab = 
    let inv_tab = M.fromListWith (++) $ do
            (c,xs) <- M.toList tab
            return (xs, [c])
    in  snd $ head $ M.toAscList inv_tab
  
-- | specification:  ( table vs ) M.! c = xs    iff
-- forall i : in vs, candidate c occurs (xs!!i) times at position i

table :: Ord a => Votes a -> M.Map a [Integer]
table vs = M.fromListWith (zipWith (+)) $ do
    v <- vs
    (k,d) <- zip [ 0 .. ] v
    return ( d, map ( \ e -> if d == e then 1 else 0 ) v )