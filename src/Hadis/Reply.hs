module Hadis.Reply where

class Reply a where
  replyVal :: a -> String

instance Reply () where
  replyVal () = "OK"

instance Show a => Reply (Maybe a) where
  replyVal (Just a) = show a
  replyVal Nothing  = ""

instance Reply Bool where
  replyVal True  = "1"
  replyVal False = "0"

instance Show a => Reply [a] where
  replyVal = show
