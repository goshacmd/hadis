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
getSet k = toSet . Map.lookup k
setGets k f = ensureSet k >> gets f

onSet :: (Set String -> ReplyVal) -> Key -> CommandReply
onSet f k = setGets k $ f . getSet k

---

sadd :: Key -> String -> CommandReply
sadd k v = onSet (ReplyInt . boolToInt . Set.notMember v) k
        >>= preserveModify (Map.alter (Just . ValueSet . Set.insert v . toSet) k)

scard :: Key -> CommandReply
scard = onSet $ ReplyInt . Set.size

sismember :: Key -> String -> CommandReply
sismember k v = flip onSet k $ ReplyInt . boolToInt . Set.member v

smembers :: Key -> CommandReply
smembers = onSet $ ReplyList . map (ReplyStr . Just) . Set.elems
