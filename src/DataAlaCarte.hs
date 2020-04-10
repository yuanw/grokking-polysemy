{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DataAlaCarte
  ( eval,
    pretty,
    Expr,
    Add,
    Mul,
    Val,
    (⊗),
    (⊕),
  )
where

import GHC.Generics

newtype Expr f = In (f (Expr f))

--data (f :+: g) e = Inl (f e) | Inr (g e)
--infix 8 :+:

-- e is phantom type for type safey
newtype Val e = Val Int

data Add e = Add e e

type AddExpr = Expr Add

data Mul x = Mul x x

instance Functor Val where
  fmap _ (Val e) = Val e

instance Functor Add where
  fmap f (Add left right) = Add (f left) (f right)

instance Functor Mul where
  fmap f (Mul left right) = Mul (f left) (f right)

--instance (Functor f, Functor g) => Functor (f :+:  g) where
--  fmap h (Inl l) = Inl (fmap h l)
--  fmap h (Inr r) = Inr (fmap h r)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add l r) = l + r

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (L1 l) = evalAlgebra l
  evalAlgebra (R1 r) = evalAlgebra r

instance Eval Mul where
  evalAlgebra (Mul l r) = l * r

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = L1
  prj (L1 l) = Just l
  prj (R1 _) = Nothing

--instance (Functor f, Functor g) =>  g :<: ( f :+: g) where
--  inj = Inr

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = R1 . inj
  prj (L1 _) = Nothing
  prj (R1 r) = prj r

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 ⊕

(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

infixl 7 ⊗

(⊗) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x ⊗ y = inject (Mul x y)

class Render f where
  render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add l r) = "(" ++ pretty l ++ " + " ++ pretty r ++ ")"

instance Render Mul where
  render (Mul l r) = "(" ++ pretty l ++ " * " ++ pretty r ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (L1 x) = render x
  render (R1 y) = render y

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do
  Mul a b <- match t
  Add c d <- match b
  return (a ⊗ c ⊕ a ⊗ d)
