module Hadis.Types where

---
import           Data.Map            (Map)
import           Control.Monad.State (StateT)
import           Control.Monad.Error (ErrorT, Error)
---

type Key = String
type Value = String
type KVMap = Map Key Value
type AppState = StateT KVMap IO
type ErrorState = ErrorT RedisError AppState
type CommandReply = ErrorState ReplyVal

data RedisError = WrongType
                deriving (Show, Eq)

instance Error RedisError

data ReplyVal = OK
              | IntVal Int
              | StrVal (Maybe String)
              | ListVal [String]
              deriving (Show, Eq)

data Command = DEL Key
             | RENAME Key Key
             | EXISTS Key
             | TYPE Key
             | KEYS String
             | SET Key Value
             | GET Key
             | GETSET Key Value
             | APPEND Key Value
             | STRLEN Key
             | INCR Key
             | INCRBY Key Int
             | DECR Key
             | DECRBY Key Int
             deriving (Show, Read)
