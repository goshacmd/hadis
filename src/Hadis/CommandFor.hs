module Hadis.CommandFor where

---
import Hadis.Types
import Hadis.Commands
---

commandFor :: Command -> CommandReply
commandFor (DEL k)         = del k
commandFor (RENAME o n)    = rename o n
commandFor (RENAMENX o n)  = renamenx o n
commandFor (EXISTS k)      = exists k
commandFor (TYPE k)        = kType k
commandFor (KEYS p)        = keys p
commandFor (SET k v)       = set k v
commandFor (SETNX k v)     = setnx k v
commandFor (GET k)         = get k
commandFor (MGET ks)       = mget ks
commandFor (GETSET k v)    = getset k v
commandFor (APPEND k v)    = append k v
commandFor (STRLEN k)      = strlen k
commandFor (INCR k)        = incr k
commandFor (INCRBY k i)    = incrby k i
commandFor (DECR k)        = decr k
commandFor (DECRBY k i)    = decrby k i
commandFor (LLEN k)        = llen k
commandFor (LPUSH k v)     = lpush k v
commandFor (LPUSHX k v)    = lpushx k v
commandFor (LPOP k)        = lpop k
commandFor (RPUSH k v)     = rpush k v
commandFor (RPUSHX k v)    = rpushx k v
commandFor (RPOP k)        = rpop k
commandFor (SADD k v)      = sadd k v
commandFor (SCARD k)       = scard k
commandFor (SISMEMBER k v) = sismember k v
