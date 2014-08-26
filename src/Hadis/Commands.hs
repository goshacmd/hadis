module Hadis.Commands where

---
import           Hadis.Base
import           Hadis.Util
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Control.Monad.State as S
import           Control.Monad.State (state, gets, modify)
import           Control.Arrow
import           Text.Read
import           Text.Regex.Glob.String
---

--- Commands: keys

del :: Key -> StateKVIO ()
del k = modify $ Map.delete k

keys :: String -> StateKVIO [Key]
keys pattern = gets $ filter (match pattern) . Map.keys

rename :: Key -> Key -> StateKVIO ()
rename k1 k2 = modify $ Map.mapKeys (\x -> if x == k1 then k2 else x)

exists :: Key -> StateKVIO Bool
exists k = gets $ Map.member k

kType :: Key -> StateKVIO KeyType
kType k = gets $ \m -> if Map.member k m then KeyString else KeyNone

--- Commands: strings

set :: Key -> Value -> StateKVIO ()
set k v = modify $ Map.insert k v

get :: Key -> StateKVIO (Maybe Value)
get k = gets $ Map.lookup k

getset :: Key -> Value -> StateKVIO (Maybe Value)
getset k v = state (Map.lookup k &&& Map.insert k v)

append :: Key -> Value -> StateKVIO Int
append k v = state $ first (length . fromJust) . alterAndRet (Just . (++v) . withDefault "") k

strlen :: Key -> StateKVIO Int
strlen k = gets $ length . Map.findWithDefault "" k

incr :: Key -> StateKVIO (Maybe Int)
incr k = state $ first (>>= readMaybe) . alterAndRet (fmap (show . (+1)) . readMaybe . withDefault "0") k

decr :: Key -> StateKVIO (Maybe Int)
decr k = state $ first (>>= readMaybe) . alterAndRet (fmap (show . flip (-) 1) . readMaybe . withDefault "0") k

--- Util

alterAndRet :: Ord a => (Maybe b -> Maybe b) -> a -> Map a b -> (Maybe b, Map a b)
alterAndRet f k m = (nv, i nv)
  where v = Map.lookup k m
        nv = f v
        i (Just a) = Map.insert k a m
        i Nothing  = m
