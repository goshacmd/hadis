{-# LANGUAGE FlexibleContexts #-}

module Hadis.Commands where

---
import           Hadis.Types
import           Hadis.Error (check)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe             (fromMaybe, fromJust, maybe)
import           Control.Monad.State    (MonadState, runStateT, state, gets, modify)
import           Control.Monad.Error    (MonadError, runErrorT, throwError)
import           Control.Arrow          ((&&&), first)
import           Text.Read              (readMaybe)
import           Text.Regex.Glob.String (match)
---

runCommand :: KVMap -> ErrorState a -> IO (Either RedisError a, KVMap)
runCommand s c = runStateT (runErrorT c) s

commandFor :: Command -> CommandReply
commandFor (DEL k)      = del k
commandFor (RENAME o n) = rename o n
commandFor (EXISTS k)   = exists k
commandFor (TYPE k)     = kType k
commandFor (KEYS p)     = keys p
commandFor (SET k v)    = set k v
commandFor (GET k)      = get k
commandFor (GETSET k v) = getset k v
commandFor (APPEND k v) = append k v
commandFor (STRLEN k)   = strlen k
commandFor (INCR k)     = incr k
commandFor (INCRBY k i) = incrby k i
commandFor (DECR k)     = decr k
commandFor (DECRBY k i) = decrby k i
commandFor (LLEN k)     = llen k
commandFor (LPUSH k v)  = lpush k v

ensure :: (Value -> Bool) -> Key -> ErrorState ()
ensure f k = check WrongType (maybe True f . Map.lookup k)

ensureString :: Key -> ErrorState ()
ensureString = ensure isStringVal

ensureList :: Key -> ErrorState ()
ensureList = ensure isListVal

--- Commands: keys

del :: Key -> CommandReply
del k = aOk $ Map.delete k

keys :: String -> CommandReply
keys pattern = gets $ ReplyList . filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply
rename k1 k2 = aOk $ Map.mapKeys (idUnless k1 k2)

exists :: Key -> CommandReply
exists k = gets $ boolVal . Map.member k

kType :: Key -> CommandReply
kType k = gets $ ReplyStr . Just . \m -> if Map.member k m then "string" else "none"

--- Commands: strings

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
             >> state (first (ReplyInt . length . valToString . fromJust) . alterAndRet (fmap ValueString . Just . (++v) . maybe "" valToString) k)

strlen :: Key -> CommandReply
strlen k = ensureString k
           >> gets (ReplyInt . length . maybe "" valToString . Map.lookup k)

incr :: Key -> CommandReply
incr k = incrby k 1

incrby :: Key -> Int -> CommandReply
incrby k i = ensureString k
             >> state (first ((>>= readMaybe) . fmap valToString) . alterAndRet (fmap (ValueString . show . (+i)) . readMaybe . maybe "0" valToString) k)
             >>= maybeToVal

decr :: Key -> CommandReply
decr k = decrby k 1

decrby :: Key -> Int -> CommandReply
decrby k i = ensureString k
             >> state (first ((>>= readMaybe) . fmap valToString) . alterAndRet (fmap (ValueString . show . flip (-) i) . readMaybe . maybe "0" valToString) k)
             >>= maybeToVal

--- Commands: lists

llen :: Key -> CommandReply
llen k = ensureList k
         >> gets (ReplyInt . length . maybe [] valToList . Map.lookup k)

lpush :: Key -> String -> CommandReply
lpush k v = ensureList k
            >> state (first (ReplyInt . length . valToList . fromJust) . alterAndRet (fmap ValueList . Just . (v:) . maybe [] valToList) k)

--- Util

alterAndRet :: Ord a => (Maybe b -> Maybe b) -> a -> Map a b -> (Maybe b, Map a b)
alterAndRet f k m = (nv, i nv)
  where v = Map.lookup k m
        nv = f v
        i (Just a) = Map.insert k a m
        i Nothing  = m

replyVal :: ReplyVal -> String
replyVal OK                = "OK"
replyVal (ReplyInt i)        = show i
replyVal (ReplyStr (Just s)) = show s
replyVal (ReplyStr Nothing)  = "(nil)"
replyVal (ReplyList ls)      = show ls

finalReply :: Either RedisError ReplyVal -> String
finalReply (Left e) = "ERR " ++ show e
finalReply (Right x) = replyVal x

aOk :: MonadState s m => (s -> s) -> m ReplyVal
aOk f = modify f >> return OK

boolVal :: Bool -> ReplyVal
boolVal True  = ReplyInt 1
boolVal False = ReplyInt 0

maybeToVal :: MonadError RedisError m => Maybe Int -> m ReplyVal
maybeToVal (Just i) = return $ ReplyInt i
maybeToVal Nothing  = throwError WrongType

idUnless :: Eq a => a -> a -> a -> a
idUnless k1 k2 x = if x == k1 then k2 else x
