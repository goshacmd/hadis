module Hadis.Commands.Sets
  ( sadd
  , scard
  , sismember
  , smembers
  ) where

---
import           Hadis.Util
import           Hadis.Util.Commands
import           Hadis.Ext.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
---

toSet = maybe Set.empty valToSet

onSet :: (Set String -> ReplyVal) -> Key -> CommandReply
onSet = on isSetVal toSet

---

sadd :: Key -> String -> CommandReply
sadd k v = onSet (replyBool . Set.notMember v) k
        >>= preserveModify (Map.alter (Just . ValueSet . Set.insert v . toSet) k)

scard :: Key -> CommandReply
scard = onSet $ ReplyInt . Set.size

sismember :: Key -> String -> CommandReply
sismember k v = flip onSet k $ replyBool . Set.member v

smembers :: Key -> CommandReply
smembers = onSet $ ReplyList . map (ReplyStr . Just) . Set.elems
