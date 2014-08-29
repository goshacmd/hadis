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

sadd :: Key -> String -> CommandReply
sadd k v = ensureSet k
        >> gets (ReplyInt . boolToInt . Set.notMember v . getSet k)
        >>= preserveModify (Map.alter (fmap ValueSet . Just . Set.insert v . toSet) k)

scard :: Key -> CommandReply
scard k = ensureSet k
        >> gets (ReplyInt . Set.size . getSet k)

sismember :: Key -> String -> CommandReply
sismember k v = ensureSet k
              >> gets (ReplyInt . boolToInt . Set.member v . getSet k)
