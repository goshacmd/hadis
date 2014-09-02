module Hadis.Commands.Keys
  ( del
  , keys
  , rename
  , renamenx
  , exists
  , kType
  ) where

---
import           Hadis.Util.Commands
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Regex.Glob.String (match)
---

del :: Key -> CommandReply
del = aOk . Map.delete

keys :: String -> CommandReply
keys pattern = gets $ ReplyList . map (ReplyStr . Just) . filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply
rename k1 k2 = aOk $ Map.mapKeys (idUnless k1 k2)

renamenx :: Key -> Key -> CommandReply
renamenx k1 k2 = nx k2 $ Map.mapKeys (idUnless k1 k2)

exists :: Key -> CommandReply
exists k = gets $ ReplyInt . boolToInt . Map.member k

kType :: Key -> CommandReply
kType k = gets $ ReplyStr . Just . maybe "none" valType . Map.lookup k
