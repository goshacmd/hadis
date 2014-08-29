module Hadis.Commands.Lists
  ( llen
  , lpush
  , lpop
  , rpush
  ) where

---
import           Hadis.Util.Commands
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Arrow ((&&&))
---

toList = maybe [] valToList
toVal = fmap ValueList

alterList :: ([String] -> [String])
             -> Key
             -> KVMap
             -> (ReplyVal, KVMap)
alterList f = kvAlter (f . toList) (toVal . Just) (ReplyInt . length)

llen :: Key -> CommandReply
llen k = ensureList k
       >> gets (ReplyInt . length . toList . Map.lookup k)

lpush :: Key -> String -> CommandReply
lpush k v = ensureList k
          >> state (alterList (v:) k)

lpop :: Key -> CommandReply
lpop k = ensureList k
       >> state (\m ->
           let (h, t) = (maybeHead &&& maybeTail) . toList $ Map.lookup k m
               nm = Map.alter (const $ toVal t) k m
           in (ReplyStr h, nm))

rpush :: Key -> String -> CommandReply
rpush k v = ensureList k
          >> state (alterList (++ [v]) k)
