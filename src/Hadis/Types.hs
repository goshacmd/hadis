module Hadis.Types where

---
import           Data.Map            (Map)
import           Data.Set            (Set)
import           Control.Monad.State (StateT)
import           Control.Monad.Error (ErrorT, Error)
---

type Key = String
type KVMap = Map Key Value
type AppState = StateT KVMap IO
type ErrorState = ErrorT RedisError AppState
type CommandReply = ErrorState ReplyVal

data Value = ValueString { valToString :: String }
           | ValueList   { valToList   :: [String] }
           | ValueSet    { valToSet    :: Set String }
           deriving (Show, Eq)

data RedisError = WrongType
                deriving (Show, Eq)

instance Error RedisError

data ReplyVal = OK
              | ReplyInt Int
              | ReplyStr (Maybe String)
              | ReplyList [ReplyVal]
              deriving (Show, Eq)

data Command = DEL Key
             | RENAME Key Key
             | RENAMENX Key Key
             | EXISTS Key
             | TYPE Key
             | KEYS String
             | SET Key String
             | SETNX Key String
             | GET Key
             | GETSET Key String
             | APPEND Key String
             | STRLEN Key
             | INCR Key
             | INCRBY Key Int
             | DECR Key
             | DECRBY Key Int
             | LLEN Key
             | LPUSH Key String
             | LPUSHX Key String
             | LPOP Key
             | RPUSH Key String
             | RPUSHX Key String
             | RPOP Key
             | SADD Key String
             | SCARD Key
             | SISMEMBER Key String
             deriving (Show, Read)

valType :: Value -> String
valType (ValueString _) = "string"
valType (ValueList _)   = "list"
valType (ValueSet _)    = "set"

isStringVal :: Value -> Bool
isStringVal x = valType x == "string"

isListVal :: Value -> Bool
isListVal x = valType x == "list"

isSetVal :: Value -> Bool
isSetVal x = valType x == "set"
