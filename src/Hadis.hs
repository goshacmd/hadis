---
import           Hadis.Base
import           Hadis.Commands
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

runCommand :: Command -> CommandReply
runCommand (DEL k)      = del k
runCommand (RENAME o n) = rename o n
runCommand (EXISTS k)   = exists k
runCommand (TYPE k)     = kType k
runCommand (KEYS p)     = keys p
runCommand (SET k v)    = set k v
runCommand (GET k)      = get k
runCommand (GETSET k v) = getset k v
runCommand (APPEND k v) = append k v
runCommand (STRLEN k)   = strlen k
runCommand (INCR k)     = incr k
runCommand (DECR k)     = decr k

repl :: StateKVIO ()
repl = do
  line <- liftIO prompt
  m <- S.get

  let command = readMaybe line :: Maybe Command

  if isJust command then do
    (r, n) <- liftIO $ runStateT (runCommand (fromJust command)) m >>= fff
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
