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

type ErrorMessage = String

data ReplyVal = OK
              | Err ErrorMessage
              | IntVal Int
              | StrVal String
              | ListVal [String]
              deriving (Show, Eq)

type CommandReply = StateKVIO ReplyVal

--- Commands: keys

del :: Key -> CommandReply
del k = aOk $ Map.delete k

keys :: String -> CommandReply
keys pattern = gets $ ListVal . filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply
rename k1 k2 = aOk $ Map.mapKeys (\x -> if x == k1 then k2 else x)

exists :: Key -> CommandReply
exists k = gets $ boolVal . Map.member k

kType :: Key -> CommandReply
kType k = gets $ StrVal . \m -> if Map.member k m then "string" else "none"

--- Commands: strings

set :: Key -> Value -> CommandReply
set k v = aOk $ Map.insert k v

get :: Key -> CommandReply
get k = gets $ toStr . Map.lookup k

getset :: Key -> Value -> CommandReply
getset k v = state (toStr . Map.lookup k &&& Map.insert k v)

append :: Key -> Value -> CommandReply
append k v = state $ first (IntVal . length . fromJust) . alterAndRet (Just . (++v) . withDefault "") k

strlen :: Key -> CommandReply
strlen k = gets $ IntVal . length . Map.findWithDefault "" k

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
replyVal OK              = "OK"
replyVal (Err msg)       = "ERR " ++ msg
replyVal (IntVal i)      = show i
replyVal (StrVal s)      = show s
replyVal (ListVal ls)    = show ls

aOk f = modify f >> return OK

toStr :: Maybe String -> ReplyVal
toStr = StrVal . withDefault ""

boolVal :: Bool -> ReplyVal
boolVal True  = IntVal 1
boolVal False = IntVal 0

maybeToVal :: Maybe Int -> ReplyVal
maybeToVal = withDefault (Err "") . fmap IntVal
