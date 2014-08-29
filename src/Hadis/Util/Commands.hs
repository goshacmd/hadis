{-# LANGUAGE FlexibleContexts #-}

module Hadis.Util.Commands
  ( module Hadis.Util.Commands
  , state
  , gets
  , modify
  ) where

---
import           Hadis.Types
import           Hadis.Ext.Error
import           Data.Map            (Map)
import qualified Data.Map as Map
import           Data.Maybe          (fromMaybe, fromJust, maybe)
import           Control.Monad.State (MonadState, state, gets, modify)
import           Control.Monad.Error (MonadError, throwError)
---

alterAndRet :: Ord a => (Maybe b -> Maybe b) -> a -> Map a b -> (Maybe b, Map a b)
alterAndRet f k m = (nv, i nv)
  where v = Map.lookup k m
        nv = f v
        i (Just a) = Map.insert k a m
        i Nothing  = m

kvAlter :: Ord k => (Maybe v -> a) -> (a -> Maybe v) -> (a -> b) -> k -> Map k v -> (b, Map k v)
kvAlter va av ab k m = (ab nv, Map.alter (const $ av nv) k m)
  where v = Map.lookup k m
        nv = va v

aOk :: MonadState s m => (s -> s) -> m ReplyVal
aOk f = modify f >> return OK

maybeToVal :: MonadError RedisError m => Maybe Int -> m ReplyVal
maybeToVal = maybeToResult WrongType ReplyInt

ensure :: (Value -> Bool) -> Key -> ErrorState ()
ensure f k = check WrongType (maybe True f . Map.lookup k)

ensureString :: Key -> ErrorState ()
ensureString = ensure isStringVal

ensureList :: Key -> ErrorState ()
ensureList = ensure isListVal

ensureSet :: Key -> ErrorState ()
ensureSet = ensure isSetVal
