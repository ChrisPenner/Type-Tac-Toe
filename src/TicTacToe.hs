{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}
{-# language ConstraintKinds #-}
module TicTacToe where

import Data.Function ((&))
import Data.Proxy
import GHC.TypeLits
import Linear.V3
import Linear.Vector
import Data.Functor.Rep
import Data.Distributive
import Data.Bifunctor

-- | Keep a list of each Piece played and its location
data BoardRep = Empty
              | Cons Nat Nat Piece BoardRep

-- | Either X, O, or Nothing
data Piece = X | O | N
  deriving (Show, Eq)

-- | Board is 3x3
type Coord n = (n <= 2, KnownNat n)

-- | Is it the proper turn for p to play a piece?
type family IsTurn (p :: Piece) (b :: BoardRep) where
  IsTurn X b = Count X b ~ Count O b
  IsTurn O b = Count X b ~ (Count O b + 1)

-- | Count amount of a certain piece on the board
type family Count (t :: Piece) (l :: BoardRep) :: Nat where
  Count _ Empty = 0
  Count t (Cons _ _ t rest) = 1 + Count t rest
  Count t (Cons _ _ _ rest) = Count t rest

-- | Has a square been played already?
type family Played (m :: Nat) (n :: Nat) (l :: BoardRep) :: Bool where
  Played _ _ Empty = False
  Played m n (Cons m n _ _) = True
  Played m n (Cons _ _ _ rest) = Played m n rest

-- | A Board is a 3x3 vector
newtype Board x a = Board (V3 (V3 a))
  deriving (Functor, Show, Eq)

instance Distributive (Board x) where
  distribute = distributeRep

instance Representable (Board x) where
  type Rep (Board x) = (Integer, Integer)
  index (Board vs) (x, y) = indexVector y $ indexVector x vs
  tabulate d = Board (V3 (V3 (d (0, 0)) (d (0, 1)) (d (0, 2)))
                         (V3 (d (1, 0)) (d (1, 1)) (d (1, 2)))
                         (V3 (d (2, 0)) (d (2, 1)) (d (2, 2)))
                     )

indexVector :: Integer -> V3 a -> a
indexVector 0 (V3 a _ _) = a
indexVector 1 (V3 _ a _) = a
indexVector 2 (V3 _ _ a) = a

newBoard :: Board Empty Piece
newBoard = Board $ V3 (V3 N N N)
                      (V3 N N N)
                      (V3 N N N)

class APiece x where
  pieceVal :: Proxy x -> Piece

instance APiece X where
  pieceVal _ = X

instance APiece O where
  pieceVal _ = O

-- | Play a piece on square (m, n) if it's valid to do so
play ::
  ( Coord m
  , Coord n
  , IsTurn p b
  , APiece p
  , Played m n b ~ False
  ) => Proxy (p :: Piece) -> (Proxy m, Proxy n) -> Board b Piece -> Board (Cons m n p b) Piece
play (pieceVal -> p) (bimap natVal natVal -> ind) b = tabulate go
  where
    go i
      | i == ind = p
      | otherwise = index b i

playX ::
  ( Coord m
  , Coord n
  , IsTurn X b
  , Played m n b ~ False
  ) => (Proxy m, Proxy n) -> Board b Piece -> Board (Cons m n X b) Piece
playX = play (Proxy :: Proxy X)

playO ::
  ( Coord m
  , Coord n
  , IsTurn O b
  , Played m n b ~ False
  ) => (Proxy m, Proxy n) -> Board b Piece -> Board (Cons m n O b) Piece
playO = play (Proxy :: Proxy O)

p :: Proxy x
p = Proxy

clearType :: Board b a -> Board () a
clearType (Board b) = Board b

game :: Board () Piece
game = newBoard
     & playX (p :: Proxy 1, p :: Proxy 2)
     & playO (p :: Proxy 2, p :: Proxy 2)
     & playX (p :: Proxy 0, p :: Proxy 2)
     & clearType
