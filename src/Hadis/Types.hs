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
             | LLEN Key
             | LPUSH Key String
             | LPOP Key
             | SADD Key String
             | SCARD Key
             deriving (Show, Read)

isStringVal :: Value -> Bool
isStringVal (ValueString _) = True
isStringVal _               = False

isListVal :: Value -> Bool
isListVal (ValueList _) = True
isListVal _             = False

isSetVal :: Value -> Bool
isSetVal (ValueSet _) = True
isSetVal _            = False
