module Hadis.Ext.Error where

---
import Control.Monad       (unless)
import Control.Monad.State (MonadState, get)
import Control.Monad.Error (MonadError, Error, throwError)
---

check :: (MonadError e m, MonadState s m) => e -> (s -> Bool) -> m ()
check e f = do
  m <- get
  unless (f m) $ throwError e

maybeToResult :: MonadError e m => e -> (a -> b) -> Maybe a -> m b
maybeToResult _ f (Just a) = return $ f a
maybeToResult e _ Nothing  = throwError e
