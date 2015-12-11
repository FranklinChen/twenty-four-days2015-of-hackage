module MonadLoopsExample where

import Control.Monad (liftM)
import Control.Monad.Loops (whileM_, unfoldM)

-- | Print prompt and read lines with retry prompts until the password
-- is correctly entered, then print congratulations.
logIn :: IO ()
logIn = do
  putStrLn "% Enter password:"
  go
  putStrLn "$ Congratulations!"

  where
    -- Use recursion for loop
    go = do
      guess <- getLine
      if guess /= "secret"
        then do
          putStrLn "% Wrong password!"
          putStrLn " Try again:"
          go
        else
          return ()

-- | No explicit recursion
logIn2 :: IO ()
logIn2 = do
  putStrLn "% Enter password:"
  whileM_ (do
             guess <- getLine
             return (guess /= "secret")
          ) (do
               putStrLn "% Wrong password!"
               putStrLn "% Try again:"
            )
  putStrLn "$ Congratulations!"

-- | With $ syntax.
logIn3 :: IO ()
logIn3 = do
  putStrLn "% Enter password:"
  whileM_ (do
             guess <- getLine
             return (guess /= "secret")
          ) $ do
    putStrLn "% Wrong password!"
    putStrLn "% Try again:"
  putStrLn "$ Congratulations!"

-- | With lifting.
logIn4 :: IO ()
logIn4 = do
  putStrLn "% Enter password:"
  whileM_ (liftM (\guess -> guess /= "secret") getLine) $ do
    putStrLn "% Wrong password!"
    putStrLn "% Try again:"
  putStrLn "$ Congratulations!"

-- | With operator sectioning and <$>.
logIn5 :: IO ()
logIn5 = do
  putStrLn "% Enter password:"
  whileM_ ((/= "secret") <$> getLine) $ do
    putStrLn "% Wrong password!"
    putStrLn "% Try again:"
  putStrLn "$ Congratulations!"

-- | Read and collect lines from stdin until encountering "quit".
readLinesUntilQuit :: IO [String]
readLinesUntilQuit = do
  line <- getLine
  if line /= "quit"
    then do
      -- recursive call, to loop
      restOfLines <- readLinesUntilQuit
      return (line : restOfLines)
    else return []

-- | No explicit recursion.
readLinesUntilQuit2 :: IO [String]
readLinesUntilQuit2 = unfoldM maybeReadLine

-- | Read a single line and check whether it's "quit".
maybeReadLine :: IO (Maybe String)
maybeReadLine = do
  line <- getLine
  return (if line /= "quit"
          then Just line
          else Nothing)

readLinesUntilQuit3 :: IO [String]
readLinesUntilQuit3 = unfoldM (notQuit <$> getLine)

notQuit :: String -> Maybe String
notQuit line =
  if line /= "quit"
    then Just line
    else Nothing
