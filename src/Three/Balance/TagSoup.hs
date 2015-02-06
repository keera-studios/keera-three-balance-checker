{-# LANGUAGE PatternGuards #-}
-- | Parses Three.co.uk's PayG Allowance website looking for the balance and
-- the expiration time
module Three.Balance.TagSoup
  ( findBroadbandData
  , findTotalBroadbandData
  )
 where

import Data.List
import Text.HTML.TagSoup

-- | Tries to find the remaining allowance and expiration time
findBroadbandData :: [Tag String] -> Maybe (String, Int)
findBroadbandData ts
  | [TagText exp,TagText amount] <- ts3
  , "Expires" `isPrefixOf` exp
  , [(n,_)] <- reads amount
  = Just (drop 7 exp, n)

  | otherwise
  = Nothing

  where ts0 = map removeBlanks ts
        ts1 = dropWhile (not . matchMobileBroadband) ts0
        ts2 = filter matchText ts1
        ts3 = drop 1 $ take 3 $ filter matchNonEmpty ts2
  
-- | Matches the tag that precedes the one with the allowance
matchMobileBroadband :: Tag String -> Bool
matchMobileBroadband (TagText "MobileBroadband") = True
matchMobileBroadband _                           = False

-- | Tries to find the total remaining allowance
findTotalBroadbandData :: [Tag String] -> Maybe Int
findTotalBroadbandData ts
  | [TagText amount] <- ts3
  , [(n,_)] <- reads amount
  = Just n
  | otherwise
  = Nothing

  where ts0 = map removeBlanks ts
        ts1 = dropWhile (not . matchTotalMobileBroadband) ts0
        ts2 = filter matchText ts1
        ts3 = drop 1 $ take 2 $ filter matchNonEmpty ts2

-- | Matches the tag that precedes the one with the total allowance
matchTotalMobileBroadband :: Tag String -> Bool
matchTotalMobileBroadband (TagText "Totalmobilebroadband(mb)") = True
matchTotalMobileBroadband _                                    = False

-- | Determines whether a tag is of type text
matchText :: Tag String -> Bool
matchText (TagText _) = True
matchText _           = False

-- | Determines whether a tag contains a non empty string
matchNonEmpty :: Tag String -> Bool
matchNonEmpty (TagText (_:_)) = True
matchNonEmpty _               = False

-- | Removes blanks from a TagText
removeBlanks :: Tag String -> Tag String
removeBlanks (TagText string) = TagText (filter notBlank string)
removeBlanks t = t

-- | Identifies non-blank characters
notBlank :: Char -> Bool
notBlank '\n' = False
notBlank '\r' = False
notBlank '\t' = False
notBlank ' '  = False
notBlank _    = True
