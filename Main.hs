{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i =
  runOutputMonoid pure
    . runInputList i
    . reinterpret2 \case
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _ -> writeTTY i >> echo

-- Let's pretend
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

-- echo forever
main :: IO ()
main = runM . teletypeToIO $ echo
