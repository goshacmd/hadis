module Hadis.Reply where

import Hadis.Base

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

instance Reply KeyType where
  replyVal KeyString = "string"
  replyVal KeyNone   = "none"

instance Show a => Reply [a] where
  replyVal = show
