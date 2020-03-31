{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Monad ((>=>))

-- https://reasonablypolymorphic.com/polysemy-talk/
data Teletype a
  = WriteLine String a
  | ReadLine (String -> a)

data Free f k
  = Pure k
  | Impure (f (Free f k))

instance Functor f => Functor (Free f) where
  fmap f (Pure k) = Pure (f k)
  fmap f (Impure a) = Impure $ (fmap . fmap) f a

instance Functor f => Applicative (Free f) where
  pure = Pure

  -- f :: a -> b
  -- b :: Free f k (a)
  Pure f <*> b = fmap f b
  -- x :: f (a -> b)
  -- y :: Free f k (a)
  Impure x <*> y = Impure (fmap (<*> y) x)

instance Functor f => Monad (Free f) where
  Pure k >>= f = f k
  Impure z >>= f = Impure $ fmap (>>= f) z

readLine :: Free Teletype String
readLine = Impure $ ReadLine pure

writeLine :: String -> Free Teletype ()
writeLine msg = Impure $ WriteLine msg $ pure ()

echo :: Free Teletype ()
echo = do
  msg <- readLine
  writeLine msg

instance Functor Teletype where
  fmap f (WriteLine msg t) = WriteLine msg $ f t
  fmap f (ReadLine g) = ReadLine (f . g)

runFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
runFree _ (Pure a) = pure a
runFree f (Impure k) = f k >>= runFree f

runTeletypeInIO :: Free Teletype a -> IO a
runTeletypeInIO = runFree $ \case
  WriteLine msg k -> do
    putStrLn msg
    pure k
  ReadLine k -> k <$> getLine

main :: IO ()
main = runTeletypeInIO echo
