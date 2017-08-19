{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
module Lib where

import GHC.TypeLits
import Linear.V3
import Linear.Vector
import Data.Functor.Rep
import Data.Distributive

data Coord = A | B | C
  deriving (Show, Eq)

data Piece = X | O | N
  deriving (Show, Eq)

data List xs where
    Nil :: List '[]
    (:::) :: a -> List as -> List (a ': as)

infixr 6 :::

type family Count (t :: Piece) (l :: List xs) :: Nat where
  Count _ Nil = 0
  Count t (t ::: as) = 1 + Count X as
  Count t (_ ::: as) = Count X as

newtype Board x a = Board (V3 (V3 a))
  deriving (Functor, Show, Eq)

instance Distributive (Board x) where
  distribute = distributeRep

indexVector :: Coord -> V3 a -> a
indexVector A (V3 a _ _) = a
indexVector B (V3 _ a _) = a
indexVector C (V3 _ _ a) = a

instance Representable (Board x) where
  type Rep (Board x) = (Coord, Coord)
  index (Board vs) (x, y) = indexVector y $ indexVector x vs
  tabulate d = Board (V3 (V3 (d (A, A)) (d (A, B)) (d (A, C)))
                         (V3 (d (B, A)) (d (B, B)) (d (B, C)))
                         (V3 (d (C, A)) (d (C, B)) (d (C, C)))
                     )

newBoard :: Board Nil Piece
newBoard = Board $ V3 (V3 N N N)
                      (V3 N N N)
                      (V3 N N N)

playX ::
  (Count X b ~ Count O b)
  => Rep (Board b) -> Board b Piece -> Board (X ::: b) Piece
playX ind b = tabulate go
  where
    go i
      | i == ind = X
      | otherwise = index b i

playO ::
  (Count X b ~ (1 + Count O b))
  => Rep (Board b) -> Board b Piece -> Board (O ::: b) Piece
playO ind b = tabulate go
  where
    go i
      | i == ind = O
      | otherwise = index b i
