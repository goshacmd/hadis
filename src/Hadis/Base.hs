module Hadis.Base where

---
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State hiding (get)
import           Control.Arrow
---

type Key = String
type Value = String
type KVMap = Map Key Value
type StateKVIO = StateT KVMap IO

set :: Key -> Value -> StateKVIO ()
set k v = state $ \m -> ((), Map.insert k v m)

get :: Key -> StateKVIO (Maybe Value)
get k = state $ \m -> (Map.lookup k m, m)

getset :: Key -> Value -> StateKVIO (Maybe Value)
getset k v = state (Map.lookup k &&& Map.insert k v)

del :: Key -> StateKVIO ()
del k = state $ \m -> ((), Map.delete k m)

keys :: StateKVIO [Key]
keys = state $ \m -> (Map.keys m, m)

rename :: Key -> Key -> StateKVIO ()
rename k1 k2 = state $ \m -> ((), Map.mapKeys (\x -> if x == k1 then k2 else x) m)
