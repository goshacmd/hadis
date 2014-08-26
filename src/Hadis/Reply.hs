{-# LANGUAGE FlexibleInstances #-}

module Hadis.Reply where

import Hadis.Base

class Reply a where
  replyVal :: a -> String

instance Reply () where
  replyVal () = "OK"

instance Show a => Reply (Maybe a) where
  replyVal (Just a) = show a
  replyVal Nothing  = show ""

instance Reply a => Reply (Either String a) where
  replyVal (Left e) = "ERROR: " ++ e
  replyVal (Right a) = replyVal a

instance Reply Bool where
  replyVal True  = "1"
  replyVal False = "0"

instance Reply Int where
  replyVal = show

instance Reply KeyType where
  replyVal KeyString = "string"
  replyVal KeyNone   = "none"

instance Show a => Reply [a] where
  replyVal = show
