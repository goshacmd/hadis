module Hadis.Commands.Sets
  ( sadd
  , scard
  , sismember
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

sadd :: Key -> String -> CommandReply
sadd k v = setGets k (ReplyInt . boolToInt . Set.notMember v . getSet k)
        >>= preserveModify (Map.alter (Just . ValueSet . Set.insert v . toSet) k)

scard :: Key -> CommandReply
scard k = setGets k $ ReplyInt . Set.size . getSet k

sismember :: Key -> String -> CommandReply
sismember k v = setGets k $ ReplyInt . boolToInt . Set.member v . getSet k
