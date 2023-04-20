{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module TwentyFortyEight
  ( run2048,
  )
where

import Brick
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Data.Word
import GHC.Generics (Generic)
import System.Random
import Text.Printf

newtype Board = Board {boardPieces :: Vector Power}
  deriving (Show, Eq, Generic)

emptyBoard :: Board
emptyBoard = Board $ V.replicate 16 (Power 0)

boardRows :: Board -> [Vector Power]
boardRows = vectorChunksOf 4 . boardPieces

vectorChunksOf :: Int -> Vector a -> [Vector a]
vectorChunksOf i = go
  where
    go :: Vector a -> [Vector a]
    go v =
      let (c, rest) = V.splitAt i v
       in if V.length rest <= i
            then [c, rest]
            else c : go rest

renderBoard :: Board -> String
renderBoard b = unlines $ flip map (boardRows b) $ \row ->
  unwords $ map renderPower (V.toList row)

boardSetTile :: TileIx -> Power -> Board -> Board
boardSetTile ix p (Board v) = Board $ V.modify (setTile ix p) v

setTile :: TileIx -> a -> (forall s. MVector s a -> ST s ())
setTile (TileIx ix) power = \v -> MV.write v ix power

newtype TileIx = TileIx {unTileIx :: Int}
  deriving (Show, Eq, Generic)

newtype Power = Power {unPower :: Word8}
  deriving (Show, Eq, Generic)

renderPower :: Power -> String
renderPower = \case
  Power 0 -> "    "
  p -> printf "%4d" $ powerValue p

powerValue :: Power -> Word
powerValue = (2 ^) . unPower

type GameT m a = StateT GameState m a

type GameState = StdGen

genTileIx :: Monad m => GameT m TileIx
genTileIx = TileIx <$> state (randomR (0, 15))

generateStartingBoard :: Monad m => GameT m Board
generateStartingBoard = do
  firstTile <- genTileIx
  secondTile <- do
    TileIx candidate <- genTileIx
    pure $
      TileIx $
        if TileIx candidate == firstTile
          then (candidate + 1) `mod` 16
          else candidate

  pure $ boardSetTile firstTile (Power 1) $ boardSetTile secondTile (Power 1) emptyBoard

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

twentyFortyEightApp :: App GameState e ResourceName
twentyFortyEightApp = undefined

run2048 :: IO ()
run2048 = do
  let initState = mkStdGen 42
  flip evalStateT initState $ do
    b <- generateStartingBoard
    liftIO $ putStrLn $ renderBoard b
  void $ Brick.defaultMain twentyFortyEightApp
