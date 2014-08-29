module Hadis.Error where

---
import Control.Monad       (unless)
import Control.Monad.State (MonadState, get)
import Control.Monad.Error (MonadError, Error, throwError)
---

check :: (MonadError e m, MonadState s m) => e -> (s -> Bool) -> m ()
check e f = do
  m <- get
  unless (f m) $ throwError e
