module Hadis.Commands.Strings
  ( set
  , setnx
  , get
  , mget
  , getset
  , append
  , strlen
  , incr
  , incrby
  , decr
  , decrby
  ) where

---
import           Hadis.Util.Commands
import           Data.Map      (Map)
import qualified Data.Map as Map
import           Text.Read     (readMaybe)
import           Control.Arrow ((&&&))
import           Control.Monad (join)
---

toStr = maybe "" valToString
toInt = readMaybe . maybe "0" valToString

alterStr f = kvAlter (f . toStr) (Just . ValueString)
alterInt f = kvAlter (fmap f . toInt) (fmap ValueString) (>>= readMaybe)

strGets k f = ensureString k >> gets f
strState k f = ensureString k >> state f

set :: Key -> String -> CommandReply
set k = aOk . Map.insert k . ValueString

setnx :: Key -> String -> CommandReply
setnx k = nx k . Map.insert k . ValueString

get :: Key -> CommandReply
get k = strGets k $ ReplyStr . fmap valToString . Map.lookup k

mget :: [Key] -> CommandReply
mget ks = gets $ \m -> ReplyList $ map (strOrNil . flip Map.lookup m) ks

getset :: Key -> String -> CommandReply
getset k v = strState k $ ReplyStr . fmap valToString . Map.lookup k &&& Map.insert k (ValueString v)

append :: Key -> String -> CommandReply
append k v = strState k $ alterStr (++v) (ReplyInt . length) k

strlen :: Key -> CommandReply
strlen k = strGets k $ ReplyInt . length . toStr . Map.lookup k

incr :: Key -> CommandReply
incr k = incrby k 1

incrby :: Key -> Int -> CommandReply
incrby k i = strState k (alterInt (show . (+i)) k)
           >>= maybeToVal

decr :: Key -> CommandReply
decr k = decrby k 1

decrby :: Key -> Int -> CommandReply
decrby k i = strState k (alterInt (show . flip (-) i) k)
           >>= maybeToVal
