{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}
{-# language ConstraintKinds #-}
module TicTacToe where

import Data.Function ((&))
import GHC.TypeLits
import Linear.V3
import Linear.Vector
import Data.Bifunctor

-- | Keep a list of each Piece played and its location
data BoardRep = Empty
              | Cons CoordT CoordT PieceT BoardRep

-- | Either X, O, or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

data Piece a where
  X' :: Piece X
  O' :: Piece O
  N' :: Piece N

pieceT :: Piece a -> PieceT
pieceT X' = X
pieceT O' = O
pieceT N' = N

data CoordT = A | B | C
  deriving (Show, Eq)

data Coord (a :: CoordT) where
  A' :: Coord A
  B' :: Coord B
  C' :: Coord C

coordT :: Coord a -> CoordT
coordT A' = A
coordT B' = B
coordT C' = C

-- | Is it the proper turn for p to play a piece?
type family IsTurn (p :: PieceT) (b :: BoardRep) where
  IsTurn X b = Count X b ~ Count O b
  IsTurn O b = Count X b ~ (Count O b + 1)

-- | Count amount of a certain piece on the board
type family Count (t :: PieceT) (l :: BoardRep) :: Nat where
  Count _ Empty = 0
  Count t (Cons _ _ t rest) = 1 + Count t rest
  Count t (Cons _ _ _ rest) = Count t rest

-- | Has a square been played already?
type family Played (m :: CoordT) (n :: CoordT) (l :: BoardRep) :: Bool where
  Played _ _ Empty = False
  Played m n (Cons m n _ _) = True
  Played m n (Cons _ _ _ rest) = Played m n rest

-- | A Board is a 3x3 vector
newtype Board x a = Board (V3 (V3 a))
  deriving (Functor, Show, Eq)

index :: Board b a -> (CoordT, CoordT) -> a
index (Board vs) (x, y) = indexVector y $ indexVector x vs

tabulate :: ((CoordT, CoordT) -> a) -> Board b a
tabulate d = Board (V3 (V3 (d (A, A)) (d (A, B)) (d (A, C)))
                        (V3 (d (B, A)) (d (B, B)) (d (B, C)))
                        (V3 (d (C, A)) (d (C, B)) (d (C, C)))
                   )

indexVector :: CoordT -> V3 a -> a
indexVector A (V3 a _ _) = a
indexVector B (V3 _ a _) = a
indexVector C (V3 _ _ a) = a

newBoard :: Board Empty PieceT
newBoard = Board $ V3 (V3 N N N)
                      (V3 N N N)
                      (V3 N N N)

-- | Play a piece on square (m, n) if it's valid to do so
play ::
  ( IsTurn p b
  , Played m n b ~ False
  ) => Piece p -> (Coord m, Coord n) -> Board b PieceT -> Board (Cons m n p b) PieceT

play (pieceT -> p) (coordT -> x, coordT -> y) b = tabulate go
  where
    go i
      | i == (x, y) = p
      | otherwise = index b i

playX ::
  ( IsTurn X b
  , Played m n b ~ False
  ) => (Coord m, Coord n) -> Board b PieceT -> Board (Cons m n X b) PieceT
playX = play X'

playO ::
  ( IsTurn O b
  , Played m n b ~ False
  ) => (Coord m, Coord n) -> Board b PieceT -> Board (Cons m n O b) PieceT
playO = play O'

clearType :: Board b a -> Board () a
clearType (Board b) = Board b

game :: Board () PieceT
game = newBoard
     & playX (A', B')
     & playO (C', C')
     & playX (A', A')
     & clearType
