module Hadis.Util where

idUnless :: Eq a => a -> a -> a -> a
idUnless k1 k2 x = if x == k1 then k2 else x

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing

maybeTail :: [a] -> Maybe [a]
maybeTail (_:xs) = Just xs
maybeTail _      = Nothing
