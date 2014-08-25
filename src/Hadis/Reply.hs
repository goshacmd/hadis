module Hadis.Reply where

class Replyable a where
  replyVal :: a -> String

instance Replyable () where
  replyVal () = "OK"

instance Show a => Replyable (Maybe a) where
  replyVal (Just a) = show a
  replyVal Nothing = ""

instance Show a => Replyable [a] where
  replyVal = show
