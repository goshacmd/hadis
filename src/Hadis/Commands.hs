module Hadis.Commands where

---
import           Hadis.Base
import           Hadis.Util
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Control.Monad.State as S
import           Control.Monad.State (state, gets, modify)
import           Control.Arrow
import           Text.Read
import           Text.Regex.Glob.String
---

type ErrorMessage = String
type Result a = Either ErrorMessage a
type CommandReply a = StateKVIO (Result a)

aOk f = modify f >> return (Right ())
err msg = return (Left msg)
ret f = gets $ return . f

--- Commands: keys

del :: Key -> CommandReply ()
del k = aOk $ Map.delete k

keys :: String -> CommandReply [Key]
keys pattern = ret $ filter (match pattern) . Map.keys

rename :: Key -> Key -> CommandReply ()
rename k1 k2 = aOk $ Map.mapKeys (\x -> if x == k1 then k2 else x)

exists :: Key -> CommandReply Bool
exists k = ret $ Map.member k

kType :: Key -> CommandReply KeyType
kType k = ret $ \m -> if Map.member k m then KeyString else KeyNone

--- Commands: strings

set :: Key -> Value -> CommandReply ()
set k v = aOk $ Map.insert k v

get :: Key -> CommandReply (Maybe Value)
get k = ret $ Map.lookup k

getset :: Key -> Value -> CommandReply (Maybe Value)
getset k v = state (return . Map.lookup k &&& Map.insert k v)

append :: Key -> Value -> CommandReply Int
append k v = state $ first (return . length . fromJust) . alterAndRet (Just . (++v) . withDefault "") k

strlen :: Key -> CommandReply Int
strlen k = ret $ length . Map.findWithDefault "" k

incr :: Key -> CommandReply (Maybe Int)
incr k = state $ first (return . (>>= readMaybe)) . alterAndRet (fmap (show . (+1)) . readMaybe . withDefault "0") k

decr :: Key -> CommandReply (Maybe Int)
decr k = state $ first (return . (>>= readMaybe)) . alterAndRet (fmap (show . flip (-) 1) . readMaybe . withDefault "0") k

--- Util

alterAndRet :: Ord a => (Maybe b -> Maybe b) -> a -> Map a b -> (Maybe b, Map a b)
alterAndRet f k m = (nv, i nv)
  where v = Map.lookup k m
        nv = f v
        i (Just a) = Map.insert k a m
        i Nothing  = m
