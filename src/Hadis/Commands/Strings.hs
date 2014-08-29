module Hadis.Commands.Strings where

---
import           Hadis.Types
import           Hadis.Util
import           Hadis.Util.Commands
import           Data.Map      (Map)
import qualified Data.Map as Map
import           Text.Read     (readMaybe)
import           Control.Arrow ((&&&))
---

set :: Key -> String -> CommandReply
set k v = aOk $ Map.insert k (ValueString v)

get :: Key -> CommandReply
get k = ensureString k
      >> gets (ReplyStr . fmap valToString . Map.lookup k)

getset :: Key -> String -> CommandReply
getset k v = ensureString k
           >> state (ReplyStr . fmap valToString . Map.lookup k &&& Map.insert k (ValueString v))


append :: Key -> String -> CommandReply
append k v = ensureString k
           >> state (kvAlter ((++v) . maybe "" valToString) (Just . ValueString) (ReplyInt . length) k)

strlen :: Key -> CommandReply
strlen k = ensureString k
         >> gets (ReplyInt . length . maybe "" valToString . Map.lookup k)

incr :: Key -> CommandReply
incr k = incrby k 1

incrby :: Key -> Int -> CommandReply
incrby k i = ensureString k
           >> state (kvAlter (fmap (show . (+i)) . readMaybe . maybe "0" valToString) (fmap ValueString) (>>= readMaybe) k)
           >>= maybeToVal

decr :: Key -> CommandReply
decr k = decrby k 1

decrby :: Key -> Int -> CommandReply
decrby k i = ensureString k
           >> state (kvAlter (fmap (show . flip (-) i) . readMaybe . maybe "0" valToString) (fmap ValueString) (>>= readMaybe) k)
           >>= maybeToVal
