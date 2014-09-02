{-# LANGUAGE FlexibleContexts #-}

module Hadis.Util.Commands
  ( module Hadis.Util.Commands
  , module Hadis.Util
  , module Hadis.Types
  , state
  , gets
  , modify
  ) where

---
import           Hadis.Types
import           Hadis.Util
import           Hadis.Ext.Error
import           Data.Map            (Map)
import qualified Data.Map as Map
import           Data.Maybe          (fromMaybe, fromJust, maybe)
import           Control.Monad.State (MonadState, state, get, put, gets, modify)
import           Control.Monad.Error (MonadError, throwError)
---

kvAlter :: Ord k => (Maybe v -> a) -> (a -> Maybe v) -> (a -> b) -> k -> Map k v -> (b, Map k v)
kvAlter va av ab k m = (ab nv, Map.alter (const $ av nv) k m)
  where v = Map.lookup k m
        nv = va v

modifyWithResult :: MonadState s m => a -> (s -> s) -> m a
modifyWithResult a f = modify f >> return a

putWithResult :: MonadState s m => (c -> (a, s)) -> c -> m a
putWithResult f c = let (a, s) = f c in put s >> return a

aOk :: MonadState s m => (s -> s) -> m ReplyVal
aOk = modifyWithResult OK

nx :: MonadState (Map Key a) m => Key -> (Map Key a -> Map Key a) -> m ReplyVal
nx k f = do
  m <- get

  if Map.member k m
  then return $ ReplyInt 0
  else modifyWithResult (ReplyInt 1) f

x :: MonadState (Map Key a) m => Key -> (Map Key a -> ReplyVal) -> (Map Key a -> (ReplyVal, Map Key a)) -> m ReplyVal
x k f1 f = do
  m <- get

  if Map.member k m
  then putWithResult f m
  else return $ f1 m

maybeToVal :: MonadError RedisError m => Maybe Int -> m ReplyVal
maybeToVal = maybeToResult WrongType ReplyInt

strOrNil :: Maybe Value -> ReplyVal
strOrNil = ReplyStr . (>>= valToMaybeString)

ensure :: Pred -> Key -> ErrorState ()
ensure f k = check WrongType (maybe True f . Map.lookup k)

ensureString :: Key -> ErrorState ()
ensureString = ensure isStringVal

ensureList :: Key -> ErrorState ()
ensureList = ensure isListVal

replyBool :: Bool -> ReplyVal
replyBool = ReplyInt . boolToInt

on :: Pred -> (Maybe Value -> a) -> (a -> ReplyVal) -> Key -> CommandReply
on pred mapper val k = ensure pred k >> gets (val . mapper . Map.lookup k)
