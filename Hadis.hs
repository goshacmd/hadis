---
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Arrow
import           Control.Monad.State hiding (get)
---

type Key = String
type Value = String

type KVMap = Map Key Value

--- Commands

set :: Key -> Value -> State KVMap ()
set k v = state $ \m -> ((), Map.insert k v m)

get :: Key -> State KVMap (Maybe Value)
get k = state $ \m -> (Map.lookup k m, m)

getset :: Key -> Value -> State KVMap (Maybe Value)
getset k v = state (Map.lookup k &&& Map.insert k v)

---

kvm = Map.fromList [("a", "123")]

someMan :: State KVMap (Maybe Value)
someMan = do
  set "b" "456"
  set "c" "111"
  getset "a" "777"

main :: IO ()
main = print $ runState someMan kvm
