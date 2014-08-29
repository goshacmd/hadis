---
import           Hadis.Types
import           Hadis.Commands      (commandFor, runCommand, finalReply)
import qualified Data.Map as Map
import           Control.Monad.State (get, put, liftIO, evalStateT)
import           System.IO           (hFlush, stdout)
import           Text.Read           (readMaybe)
---

repl :: AppState ()
repl = do
  line <- liftIO prompt
  m <- get

  case (readMaybe line :: Maybe Command) of
    Just command -> do
      (r, n) <- liftIO . runCommand m $ commandFor command
      put n
      liftIO . putStrLn . finalReply $ r

    Nothing -> liftIO . putStrLn $ "invalid command: " ++ line

  repl

main :: IO ()
main = evalStateT repl $ Map.fromList [("a", ValueString "123")]

prompt = do
  putStr "> "
  hFlush stdout
  getLine
