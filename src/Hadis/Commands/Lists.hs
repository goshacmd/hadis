module Hadis.Commands.Lists where

---
import           Hadis.Util.Commands
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Arrow ((&&&))
---

llen :: Key -> CommandReply
llen k = ensureList k
       >> gets (ReplyInt . length . maybe [] valToList . Map.lookup k)

lpush :: Key -> String -> CommandReply
lpush k v = ensureList k
          >> state (kvAlter ((v:) . maybe [] valToList) (fmap ValueList . Just) (ReplyInt . length) k)

lpop :: Key -> CommandReply
lpop k = ensureList k
       >> state (\m ->
           let (h, t) = (maybeHead &&& maybeTail) $ maybe [] valToList $ Map.lookup k m
               nm = Map.alter (const (fmap ValueList t)) k m
           in (ReplyStr h, nm))

rpush :: Key -> String -> CommandReply
rpush k v = ensureList k
          >> state (kvAlter ((++ [v]) . maybe [] valToList) (fmap ValueList . Just) (ReplyInt . length) k)
