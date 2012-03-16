{-# LANGUAGE PatternGuards #-}
module Three.Balance.TagSoup
  ( findBroadbandData
  , findTotalBroadbandData
  )
 where

import Data.List
import Text.HTML.TagSoup

removeBlanks :: Tag String -> Tag String
removeBlanks (TagText string) = TagText (filter notBlank string)
removeBlanks t = t

notBlank :: Char -> Bool
notBlank '\n' = False
notBlank '\r' = False
notBlank '\t' = False
notBlank ' '  = False
notBlank _    = True

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
  
matchMobileBroadband :: Tag String -> Bool
matchMobileBroadband (TagText "MobileBroadband") = True
matchMobileBroadband _                           = False

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

matchTotalMobileBroadband :: Tag String -> Bool
matchTotalMobileBroadband (TagText "Totalmobilebroadband(mb)") = True
matchTotalMobileBroadband _                                    = False


matchText :: Tag String -> Bool
matchText (TagText _) = True
matchText _           = False

matchNonEmpty :: Tag String -> Bool
matchNonEmpty (TagText (_:_)) = True
matchNonEmpty _               = False
