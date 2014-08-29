module Hadis.Commands.Lists
  ( llen
  , lpush
  , lpop
  , rpush
  , rpop
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

listGets k f = ensureList k >> gets f
listState k f = ensureList k >> state f

llen :: Key -> CommandReply
llen k = listGets k $ ReplyInt . length . toList . Map.lookup k

lpush :: Key -> String -> CommandReply
lpush k v = listState k $ alterList (v:) k

lpop :: Key -> CommandReply
lpop k = listState k $ \m ->
           let (h, t) = (maybeHead &&& maybeTail) . toList $ Map.lookup k m
           in (ReplyStr h, Map.alter (const $ toVal t) k m)

rpush :: Key -> String -> CommandReply
rpush k v = listState k $ alterList (++ [v]) k

rpop :: Key -> CommandReply
rpop k = listState k $ \m ->
           let (h, t) = (maybeHead &&& maybeTail) . reverse . toList $ Map.lookup k m
           in (ReplyStr h, Map.alter (const . toVal $ fmap reverse t) k m)
