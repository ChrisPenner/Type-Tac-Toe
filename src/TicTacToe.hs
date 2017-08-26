{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}
{-# language ConstraintKinds #-}
{-# language GADTs #-}
module TicTacToe where

import Data.Function ((&))
import GHC.TypeLits

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

-- | Either X, O, or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

-- | A proxy type which represents a piece
data Piece a where
  X' :: Piece 'X
  O' :: Piece 'O
  N' :: Piece 'N

-- | Get the piece's actual value from a wrapper type
pieceVal :: Piece a -> PieceT
pieceVal X' = X
pieceVal O' = O
pieceVal N' = N

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

-- | Is it the proper turn for p to play a piece?
type family IsTurn (p :: PieceT) (b :: BoardRep) where
  IsTurn 'X b = Count 'X b ~ Count 'O b
  IsTurn 'O b = Count 'X b ~ (Count 'O b + 1)

-- | Count amount of a certain piece on the board
type family Count (t :: PieceT) (l :: BoardRep) :: Nat where
  Count _ 'Empty = 0
  Count t ('Cons _ _ t rest) = 1 + Count t rest
  Count t ('Cons _ _ _ rest) = Count t rest

-- | Has a square been played already?
type family Played (m :: CoordT) (n :: CoordT) (l :: BoardRep) :: Bool where
  Played _ _ 'Empty = 'False
  Played m n ('Cons m n _ _) = 'True
  Played m n ('Cons _ _ _ rest) = Played m n rest

-- | A Board is a 3x3 vector
newtype Board x a = Board (Trip (Trip a))
  deriving (Functor, Show, Eq)

-- | New empty board
newBoard :: Board 'Empty PieceT
newBoard = Board $ Trip (Trip N N N)
                      (Trip N N N)
                      (Trip N N N)

-- | Play a piece on square (m, n) if it's valid to do so
play ::
  ( IsTurn p b
  , Played m n b ~ 'False
  ) => Piece p -> (Coord m, Coord n) -> Board b PieceT -> Board ('Cons m n p b) PieceT
play (pieceVal -> p) (coordVal -> x, coordVal -> y) (Board b) = Board $ overTrip y (overTrip x (const p)) b

-- | Clear any associated type info about the board
clearType :: Board b a -> Board () a
clearType (Board b) = Board b

game :: Board () PieceT
game = newBoard
     & play X' (A', B')
     & play O' (C', C')
     & play X' (A', A')
     & clearType
