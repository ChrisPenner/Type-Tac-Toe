-- Adapted from a gist by mstksg: https://gist.github.com/mstksg/a46c27a3ce091d5ad43bb7f4c3669a29

{-# language DeriveFunctor #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language ViewPatterns #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
{-# language TypeInType #-}
module UsingSingletons where


import Data.Function ((&))
-- import Data.Singletons.Prelude
-- import Data.Singletons.Prelude.List
import Data.Singletons.TH

$(singletons [d|
  -- | Either X, O, or Nothing
  data Piece = X | O
    deriving (Show, Eq)

  data Coord = A | B | C
    deriving (Show, Eq)
 |])


-- | Get the coord's actual value from a wrapper type
coordVal :: SCoord a -> Coord
coordVal = fromSing

pieceVal :: SPiece a -> Piece
pieceVal = fromSing

data Trip a = Trip a a a
  deriving (Show, Eq, Functor)

-- | Utility function to alter a value inside a triple
-- Can build get / set using `flip const ()` and `const x` respectively
overTrip :: Coord -> (a -> a) -> Trip a -> Trip a
overTrip A f (Trip a b c) = Trip (f a) b c
overTrip B f (Trip a b c) = Trip a (f b) c
overTrip C f (Trip a b c) = Trip a b (f c)

-- | Keep a list of each Piece played and its location
type BoardRep = [(Coord, Coord, Piece)]


-- A board is a 3x3 grid alongside its type representation
newtype Board (b :: BoardRep) a = Board (Trip (Trip (Maybe a)))
  deriving (Show, Eq, Functor)

-- | New empty board
newBoard :: Board '[] Piece
newBoard = Board $ Trip (Trip Nothing Nothing Nothing)
                        (Trip Nothing Nothing Nothing)
                        (Trip Nothing Nothing Nothing)

-- | Has a square been played already?
type family Played (x :: Coord) (y :: Coord) (b :: BoardRep) :: Bool where
  Played _ _ '[] = 'False
  Played x y ('(x, y, _) ': _) = 'True
  Played x y ('(_, _, _) ': rest) = Played x y rest


-- | Get who's turn it is
type family Turn (b :: BoardRep) :: Piece where
  Turn ('(_, _, 'X) ': _) = 'O
  Turn _ = 'X

play
    :: (Played x y b ~ 'False, Turn b ~ p)
    => SPiece p
    -> (SCoord x, SCoord y)
    -> Board b Piece
    -> Board ('(x, y, p) ': b) Piece
play (fromSing -> p) (fromSing -> x, fromSing -> y) (Board b)
  = Board $ (overTrip y . overTrip x) (const (Just p)) b

game :: Board '[ '( 'A, 'A, 'O), '( 'A, 'B, 'X)] Piece
game = newBoard
     & play SX (SA, SB)
     & play SO (SA, SA)
