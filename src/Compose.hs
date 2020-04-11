module Compose where

import Control.Applicative (liftA2)
import Control.Monad (join)

newtype Compose f g a = Compose {getCompose :: f (g a)}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose h) = Compose $ (fmap . fmap) f h

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  Compose f <*> Compose x = Compose (liftA2 (<*>) f x)

instance (Monad f, Monad g) => Monad (Compose f g) where
  Compose x >>= mf = Compose . (fmap join =<<) . fmap (getCompose . magic) $ a
    where
      a = (fmap . fmap) mf x
      magic :: f (g a) -> g (f a)
      magic = undefined
