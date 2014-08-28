---
import           Hadis.Commands
import qualified Data.Map as Map
import           Data.Maybe
import qualified Control.Monad.State as S
import           Control.Monad.State hiding (get)
import           Control.Monad.Identity
import           Control.Monad.Error
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
             | INCRBY Key Int
             | DECR Key
             | DECRBY Key Int
             deriving (Show, Read)

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

repl :: StateKVIO ()
repl = do
  line <- liftIO prompt
  m <- S.get

  let command = readMaybe line :: Maybe Command

  if isJust command then do
    let c = fromJust command
        rc = commandFor c

    (r, n) <- liftIO $ runCommand m rc

    put n

    liftIO . putStrLn . finalReply $ r
  else
    liftIO . putStrLn $ "invalid command: " ++ line

  repl

main :: IO ()
main = evalStateT repl $ Map.fromList [("a", "123")]

prompt = do
  putStr "> "
  hFlush stdout
  getLine
