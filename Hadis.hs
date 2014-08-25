---
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Arrow
import qualified Control.Monad.State as S
import           Control.Monad.State hiding (get)
import           System.IO
---

type Key = String
type Value = String
type KVMap = Map Key Value
type StateKVIO = StateT KVMap IO

--- Commands

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

---

data Command = SET Key Value | GET Key | GETSET Key Value | DEL String | KEYS deriving (Show, Read)

fff (a, b)= return (show a, b)

runCommand :: Command -> KVMap -> IO (String, KVMap)
runCommand (SET k v)    m = runStateT (set k v) m    >>= fff
runCommand (GET k)      m = runStateT (get k)   m    >>= fff
runCommand (GETSET k v) m = runStateT (getset k v) m >>= fff
runCommand (DEL k)      m = runStateT (del k) m      >>= fff
runCommand KEYS         m = runStateT keys m         >>= fff

rc :: StateKVIO ()
rc = do
  line <- liftIO prompt
  m <- S.get
  let command = read line :: Command
  (r,n) <- liftIO . runCommand command $ m
  modify (const n)
  liftIO $ putStrLn r
  rc

kvm = Map.fromList [("a", "123")]

prompt = do
  putStr "> "
  hFlush stdout
  getLine

main :: IO ()
main = evalStateT rc kvm
