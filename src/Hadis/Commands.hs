module Hadis.Commands where

---
import           Hadis.Util
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Control.Monad.State as S
import           Control.Monad.State (StateT, state, gets, modify)
import           Control.Monad.Identity
import           Control.Monad.Error
import           Control.Arrow
import           Text.Read
import           Text.Regex.Glob.String
---

type Key = String
type Value = String
type KVMap = Map Key Value
data KeyType = KeyString | KeyNone deriving (Show)
type StateKVIO = StateT KVMap IO

data RedisError = WrongType deriving (Show, Eq)

instance Error RedisError where

data ReplyVal = OK
              | IntVal Int
              | StrVal (Maybe String)
              | ListVal [String]
              deriving (Show, Eq)

type ValOrError = ErrorT RedisError Identity ReplyVal

type CommandReply = StateKVIO ValOrError

--- Commands: keys

del :: Key -> CommandReply
del k = aOk $ Map.delete k

keys :: String -> CommandReply
keys pattern = gets $ return . ListVal . filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply
rename k1 k2 = aOk $ Map.mapKeys (\x -> if x == k1 then k2 else x)

exists :: Key -> CommandReply
exists k = gets $ return . boolVal . Map.member k

kType :: Key -> CommandReply
kType k = gets $ return . StrVal . Just . \m -> if Map.member k m then "string" else "none"

--- Commands: strings

set :: Key -> Value -> CommandReply
set k v = aOk $ Map.insert k v

get :: Key -> CommandReply
get k = gets $ return . StrVal . Map.lookup k

getset :: Key -> Value -> CommandReply
getset k v = state (return . StrVal . Map.lookup k &&& Map.insert k v)

append :: Key -> Value -> CommandReply
append k v = state $ first (return . IntVal . length . fromJust) . alterAndRet (Just . (++v) . withDefault "") k

strlen :: Key -> CommandReply
strlen k = gets $ return . IntVal . length . Map.findWithDefault "" k

incr :: Key -> CommandReply
incr k = state $ first (maybeToVal . (>>= readMaybe)) . alterAndRet (fmap (show . (+1)) . readMaybe . withDefault "0") k

decr :: Key -> CommandReply
decr k = state $ first (maybeToVal . (>>= readMaybe)) . alterAndRet (fmap (show . flip (-) 1) . readMaybe . withDefault "0") k

--- Util

alterAndRet :: Ord a => (Maybe b -> Maybe b) -> a -> Map a b -> (Maybe b, Map a b)
alterAndRet f k m = (nv, i nv)
  where v = Map.lookup k m
        nv = f v
        i (Just a) = Map.insert k a m
        i Nothing  = m

replyVal :: ReplyVal -> String
replyVal OK                = "OK"
replyVal (IntVal i)        = show i
replyVal (StrVal (Just s)) = show s
replyVal (StrVal Nothing)  = "(nil)"
replyVal (ListVal ls)      = show ls

finalReply :: Either RedisError ReplyVal -> String
finalReply (Left e) = "ERR " ++ show e
finalReply (Right x) = replyVal x

fr :: ValOrError -> String
fr = finalReply . runIdentity . runErrorT

aOk :: S.MonadState s m => (s -> s) -> m ValOrError
aOk f = modify f >> return (return OK)

boolVal :: Bool -> ReplyVal
boolVal True  = IntVal 1
boolVal False = IntVal 0

maybeToVal :: Maybe Int -> ValOrError
maybeToVal (Just i) = return $ IntVal i
maybeToVal Nothing  = throwError WrongType
