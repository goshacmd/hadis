---
import           Hadis.Types
import           Hadis.Commands      (runCommand, finalReply)
import           Hadis.CommandFor    (commandFor)
import qualified Data.Map as Map
import           Control.Monad.State (get, put, liftIO, evalStateT)
import           System.IO           (hFlush, stdout)
import           System.Console.Readline (readline, addHistory)
import           Text.Read           (readMaybe)
---

repl :: AppState ()
repl = do
  maybeLine <- liftIO $ readline "> "

  case maybeLine of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
      liftIO $ addHistory line

      case readMaybe line of
        Just command -> do
          m <- get
          (r, n) <- liftIO . runCommand m $ commandFor command
          put n
          liftIO . putStrLn . finalReply $ r

        Nothing -> liftIO . putStrLn $ "invalid command: " ++ line

      repl

main :: IO ()
main = evalStateT repl $ Map.fromList [("a", ValueString "123")]
