---
import           Hadis.Base
import           Hadis.Commands
import           Hadis.Reply
import qualified Data.Map as Map
import           Data.Maybe
import qualified Control.Monad.State as S
import           Control.Monad.State hiding (get)
import           System.IO
import           Text.Read (readMaybe)
---

data Command = DEL Key
             | RENAME Key Key
             | EXISTS Key
             | TYPE Key
             | KEYS String
             | SET Key Value
             | GET Key
             | GETSET Key Value
             | APPEND Key Value
             | STRLEN Key
             | INCR Key
             | DECR Key
             deriving (Show, Read)

fff (a, b) = return (replyVal a, b)

runCommand :: Command -> KVMap -> IO (String, KVMap)
runCommand (DEL k)      m = runStateT (del k)      m >>= fff
runCommand (RENAME o n) m = runStateT (rename o n) m >>= fff
runCommand (EXISTS k)   m = runStateT (exists k)   m >>= fff
runCommand (TYPE k)     m = runStateT (kType k)    m >>= fff
runCommand (KEYS p)     m = runStateT (keys p)     m >>= fff
runCommand (SET k v)    m = runStateT (set k v)    m >>= fff
runCommand (GET k)      m = runStateT (get k)      m >>= fff
runCommand (GETSET k v) m = runStateT (getset k v) m >>= fff
runCommand (APPEND k v) m = runStateT (append k v) m >>= fff
runCommand (STRLEN k)   m = runStateT (strlen k)   m >>= fff
runCommand (INCR k)     m = runStateT (incr k)     m >>= fff
runCommand (DECR k)     m = runStateT (decr k)     m >>= fff

repl :: StateKVIO ()
repl = do
  line <- liftIO prompt
  m <- S.get

  let command = readMaybe line :: Maybe Command

  if isJust command then do
    (r, n) <- liftIO . runCommand (fromJust command) $ m
    put n
    liftIO . putStrLn $ r
  else
    liftIO . putStrLn $ "invalid command: " ++ line

  repl

main :: IO ()
main = evalStateT repl $ Map.fromList [("a", "123")]

prompt = do
  putStr "> "
  hFlush stdout
  getLine
