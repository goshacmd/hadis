module Hadis.Types where

---
import           Data.Map            (Map)
import           Control.Monad.State (StateT)
import           Control.Monad.Error (ErrorT, Error)
---

type Key = String
type KVMap = Map Key Value
type AppState = StateT KVMap IO
type ErrorState = ErrorT RedisError AppState
type CommandReply = ErrorState ReplyVal

data Value = ValueString { valToString :: String }
           deriving (Show, Eq)

data RedisError = WrongType
                deriving (Show, Eq)

instance Error RedisError

data ReplyVal = OK
              | ReplyInt Int
              | ReplyStr (Maybe String)
              | ReplyList [String]
              deriving (Show, Eq)

data Command = DEL Key
             | RENAME Key Key
             | EXISTS Key
             | TYPE Key
             | KEYS String
             | SET Key String
             | GET Key
             | GETSET Key String
             | APPEND Key String
             | STRLEN Key
             | INCR Key
             | INCRBY Key Int
             | DECR Key
             | DECRBY Key Int
             deriving (Show, Read)

isStringVal :: Value -> Bool
isStringVal (ValueString _) = True
isStringVal _               = False
