module Hadis.Base where

---
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State (StateT, state)
import           Control.Arrow
import           Text.Read
import           Text.Regex.Glob.String
---

type Key = String
type Value = String
type KVMap = Map Key Value
data KeyType = KeyString | KeyNone deriving (Show)
type StateKVIO = StateT KVMap IO

--- Commands: keys

del :: Key -> StateKVIO ()
del k = state $ \m -> ((), Map.delete k m)

keys :: String -> StateKVIO [Key]
keys pat = state $ \m -> (filter (match pat) $ Map.keys m, m)

rename :: Key -> Key -> StateKVIO ()
rename k1 k2 = state $ \m -> ((), Map.mapKeys (\x -> if x == k1 then k2 else x) m)

exists :: Key -> StateKVIO Bool
exists k = state $ \m -> (Map.member k m, m)

kType :: Key -> StateKVIO KeyType
kType k = state $ \m -> (if Map.member k m then KeyString else KeyNone, m)

--- Commands: strings

set :: Key -> Value -> StateKVIO ()
set k v = state $ \m -> ((), Map.insert k v m)

get :: Key -> StateKVIO (Maybe Value)
get k = state $ \m -> (Map.lookup k m, m)

getset :: Key -> Value -> StateKVIO (Maybe Value)
getset k v = state (Map.lookup k &&& Map.insert k v)

append :: Key -> Value -> StateKVIO Int
append k v = state $ (length . Map.findWithDefault "" k &&& id) . Map.alter (Just . (++v) . withDefault "") k

strlen :: Key -> StateKVIO Int
strlen k = state $ \m -> (length $ Map.findWithDefault "" k m, m)

incr :: Key -> StateKVIO (Maybe Int)
incr k = state $ (readMaybeInt . Map.findWithDefault "" k &&& id) . Map.alter (fmap (show . (+1)) . readMaybeInt . withDefault "0") k

decr :: Key -> StateKVIO (Maybe Int)
decr k = state $ (readMaybeInt . Map.findWithDefault "" k &&& id) . Map.alter (fmap (show . flip (-) 1) . readMaybeInt . withDefault "0") k

--- Util

withDefault :: a -> Maybe a -> a
withDefault _ (Just a) = a
withDefault d Nothing  = d

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe
