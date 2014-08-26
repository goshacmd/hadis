module Hadis.Util where

withDefault :: a -> Maybe a -> a
withDefault _ (Just a) = a
withDefault d Nothing  = d
