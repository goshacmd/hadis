{-# LANGUAGE FlexibleContexts #-}

module Hadis.Commands where

---
import           Hadis.Types
import           Hadis.Error (check)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe             (fromMaybe, fromJust)
import           Control.Monad.State    (MonadState, runStateT, state, gets, modify)
import           Control.Monad.Error    (MonadError, runErrorT, throwError)
import           Control.Arrow          ((&&&), first)
import           Text.Read              (readMaybe)
import           Text.Regex.Glob.String (match)
---

runCommand :: KVMap -> CommandReply -> IO (Either RedisError ReplyVal, KVMap)
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

isStringValue :: Key -> KVMap -> Bool
isStringValue k m = case Map.lookup k m of
                      Just x  -> True
                      Nothing -> True -- unused key can't cause wrong type error

ensure :: (KVMap -> Bool) -> ErrorState ()
ensure = check WrongType

ensureStringValue :: Key -> ErrorState ()
ensureStringValue = ensure . isStringValue

--- Commands: keys

del :: Key -> CommandReply
del k = aOk $ Map.delete k

keys :: String -> CommandReply
keys pattern = gets $ ListVal . filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply
rename k1 k2 = aOk $ Map.mapKeys (idUnless k1 k2)

exists :: Key -> CommandReply
exists k = gets $ boolVal . Map.member k

kType :: Key -> CommandReply
kType k = gets $ StrVal . Just . \m -> if Map.member k m then "string" else "none"

--- Commands: strings

set :: Key -> Value -> CommandReply
set k v = aOk $ Map.insert k v

get :: Key -> CommandReply
get k = ensureStringValue k >> gets (StrVal . Map.lookup k)

getset :: Key -> Value -> CommandReply
getset k v = ensureStringValue k >> state (StrVal . Map.lookup k &&& Map.insert k v)

append :: Key -> Value -> CommandReply
append k v = ensureStringValue k >> state (first (IntVal . length . fromJust) . alterAndRet (Just . (++v) . fromMaybe "") k)

strlen :: Key -> CommandReply
strlen k = ensureStringValue k >> gets (IntVal . length . Map.findWithDefault "" k)

incr :: Key -> CommandReply
incr k = incrby k 1

incrby :: Key -> Int -> CommandReply
incrby k i = ensureStringValue k >> state (first (>>= readMaybe) . alterAndRet (fmap (show . (+i)) . readMaybe . fromMaybe "0") k) >>= maybeToVal

decr :: Key -> CommandReply
decr k = decrby k 1

decrby :: Key -> Int -> CommandReply
decrby k i = ensureStringValue k >> state (first (>>= readMaybe) . alterAndRet (fmap (show . flip (-) i) . readMaybe . fromMaybe "0") k) >>= maybeToVal

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

aOk :: MonadState s m => (s -> s) -> m ReplyVal
aOk f = modify f >> return OK

boolVal :: Bool -> ReplyVal
boolVal True  = IntVal 1
boolVal False = IntVal 0

maybeToVal :: MonadError RedisError m => Maybe Int -> m ReplyVal
maybeToVal (Just i) = return $ IntVal i
maybeToVal Nothing  = throwError WrongType

idUnless :: Eq a => a -> a -> a -> a
idUnless k1 k2 x = if x == k1 then k2 else x
