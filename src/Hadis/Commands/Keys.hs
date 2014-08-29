module Hadis.Commands.Keys where

---
import           Hadis.Util.Commands
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Regex.Glob.String (match)
---

del :: Key -> CommandReply
del k = aOk $ Map.delete k

keys :: String -> CommandReply
keys pattern = gets $ ReplyList . filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply
rename k1 k2 = aOk $ Map.mapKeys (idUnless k1 k2)

exists :: Key -> CommandReply
exists k = gets $ ReplyInt . boolToInt . Map.member k

kType :: Key -> CommandReply
kType k = gets $ ReplyStr . Just . maybe "none" valType . Map.lookup k
