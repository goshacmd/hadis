module Hadis.Ext.State where

---
import Control.Monad.State (MonadState, state)
---

preserveModify :: MonadState s m => (s -> s) -> a -> m a
preserveModify f a = state $ \m -> (a, f m)
