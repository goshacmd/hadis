module Hadis.Base where

---
import           Data.Map (Map)
import           Control.Monad.State (StateT)
---

type Key = String
type Value = String
type KVMap = Map Key Value
data KeyType = KeyString | KeyNone deriving (Show)
type StateKVIO = StateT KVMap IO
