module Hadis.Commands
  ( module Hadis.Commands
  , module C
  ) where

import Hadis.Commands.Keys    as C
import Hadis.Commands.Strings as C
import Hadis.Commands.Lists   as C
import Hadis.Commands.Sets    as C

---
import           Hadis.Types
import           Hadis.Util.Commands
import           Control.Monad.State    (MonadState, runStateT)
import           Control.Monad.Error    (MonadError, runErrorT)
---

runCommand :: KVMap -> ErrorState a -> IO (Either RedisError a, KVMap)
runCommand s c = runStateT (runErrorT c) s

--- Util

replyVal :: ReplyVal -> String
replyVal OK                = "OK"
replyVal (ReplyInt i)        = show i
replyVal (ReplyStr (Just s)) = show s
replyVal (ReplyStr Nothing)  = "(nil)"
replyVal (ReplyList ls)      = show ls

finalReply :: Either RedisError ReplyVal -> String
finalReply (Left e) = "ERR " ++ show e
finalReply (Right x) = replyVal x
