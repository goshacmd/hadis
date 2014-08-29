module Hadis.Commands.Sets where

---
import           Hadis.Util
import           Hadis.Util.Commands
import           Hadis.Ext.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

sadd :: Key -> String -> CommandReply
sadd k v = ensureSet k
        >> gets (ReplyInt . boolToInt . Set.notMember v . maybe Set.empty valToSet . Map.lookup k)
        >>= preserveModify (Map.alter (fmap ValueSet . Just . Set.insert v . maybe Set.empty valToSet) k)

scard :: Key -> CommandReply
scard k = ensureSet k
       >> gets (ReplyInt . Set.size . maybe Set.empty valToSet . Map.lookup k)

sismember :: Key -> String -> CommandReply
sismember k v = ensureSet k
             >> gets (ReplyInt . boolToInt . Set.member v . maybe Set.empty valToSet . Map.lookup k)
