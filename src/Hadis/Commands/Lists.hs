module Hadis.Commands.Lists where

---
import           Hadis.Types
import           Hadis.Util
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
           let v = Map.lookup k m
               (h, t) = (maybeHead &&& sureTail) $ maybe [] valToList v
               nm = Map.insert k (ValueList t) m
           in (ReplyStr h, nm))
  where maybeHead (x:_) = Just x
        maybeHead _     = Nothing
        sureTail (_:xs) = xs
        sureTail _      = []
