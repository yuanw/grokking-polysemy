module Main where

import Control.Monad ((.>=>))

-- https://reasonablypolymorphic.com/polysemy-talk/
data Teletype k
  = Done k
  | WriteLine String (Teletype k)
  | ReadLine (String -> Teletype k)

--echo :: Teletype ()
--echo = ReadLine $ \ msg -> WriteLine msg $ Done ()
echo :: Teletype ()
echo = do
  msg <- ReadLine Done
  WriteLine msg $ Done ()

runTeletypeInIO :: Teletype a -> IO a
runTeletypeInIO (Done a) = pure a
runTeletypeInIO (WriteLine msg k) = do
  putStrLn msg
  runTeletypeInIO k
runTeletypeInIO (ReadLine k) = do
  msg <- getLine
  runTeletypeInIO $ k msg

instance Functor Teletype where
  fmap f (Done g) = Done (f g)
  fmap f (WriteLine msg t) = WriteLine msg $ fmap f t
  fmap f (ReadLine g) = ReadLine (fmap f . g)

instance Applicative Teletype where
  pure = Done
  Done f <*> g = fmap f g
  (WriteLine msg f) <*> g = WriteLine msg $ f <*> g
  -- f :: String -> Teletype (a -> b)
  -- g :: Teletype a
  -- String -> Teletype b
  (ReadLine f) <*> g = ReadLine $ \msg -> f msg <*> g

instance Monad Teletype where
  Done k >>= f = f k
  WriteLine msg k >>= f = WriteLine msg $ k >>= f
  ReadLine k >>= f = ReadLine $ k .>=> f

main :: IO ()
main = runTeletypeInIO echo
