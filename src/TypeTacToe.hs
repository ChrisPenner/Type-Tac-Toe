{-# language DeriveFunctor #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language ViewPatterns #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
module TypeTacToe where

import Data.Function ((&))

-- | Either X, O, or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

data CoordT = A | B | C
  deriving (Show, Eq)

-- | A proxy type which represents a coordinate
data Coord (a :: CoordT) where
  A' :: Coord 'A
  B' :: Coord 'B
  C' :: Coord 'C

-- | Get the coord's actual value from a wrapper type
coordVal :: Coord a -> CoordT
coordVal A' = A
coordVal B' = B
coordVal C' = C

data Trip a = Trip a a a
  deriving (Show, Eq, Functor)

-- | Utility function to alter a value inside a triple
-- Can build get / set using `flip const ()` and `const x` respectively
overTrip :: CoordT -> (a -> a) -> Trip a -> Trip a
overTrip A f (Trip a b c) = Trip (f a) b c
overTrip B f (Trip a b c) = Trip a (f b) c
overTrip C f (Trip a b c) = Trip a b (f c)

-- | Keep a list of each Piece played and its location
data BoardRep = Empty
              | Cons CoordT CoordT PieceT BoardRep


-- A board is a 3x3 grid alongside its type representation
newtype Board (b :: BoardRep) a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

-- | New empty board
newBoard :: Board 'Empty PieceT
newBoard = Board $ Trip (Trip N N N)
                        (Trip N N N)
                        (Trip N N N)

-- | Has a square been played already?
type family Played (x :: CoordT) (y :: CoordT) (b :: BoardRep) :: Bool where
  Played _ _ 'Empty = 'False
  Played x y ('Cons x y _ _) = 'True
  Played x y ('Cons _ _ _ rest) = Played x y rest


-- | Get who's turn it is
type family Turn (b :: BoardRep) :: PieceT where
  Turn ('Cons _ _ 'X _) = 'O
  Turn _ = 'X

-- | Play a piece on square (x, y) if it's valid to do so
playX :: (Played x y b ~ 'False, Turn b ~ 'X)
      => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT
playX (coordVal -> x, coordVal -> y) (Board b) 
      = Board $ overTrip y (overTrip x (const X)) b

playO :: (Played x y b ~ 'False, Turn b ~ 'O)
      => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
playO (coordVal -> x, coordVal -> y) (Board b) 
      = Board $ overTrip y (overTrip x (const O)) b

game :: Board ('Cons 'A 'A 'O ('Cons 'A 'B 'X 'Empty)) PieceT
game = newBoard
     & playX (A', B')
     & playO (A', A')
