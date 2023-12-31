module BasicData where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

windowWidth :: Int
windowWidth = 1200

windowHeight :: Int
windowHeight = 800

data GameState = GameState
  { playerState :: PlayerState
  , platforms :: [Platform]
  , coins :: [Coin]
  , background :: Picture
  , score :: Int
  , level :: Int
  , gameOver :: Bool
  } deriving (Eq, Show)

data PlayerState = PlayerState
  { playerX :: Float
  , playerY :: Float
  , playerXSpeed :: Float
  , playerYSpeed :: Float
  , isJumping :: Bool
  , playerImages :: (Picture, Picture)
  , isFacingRight :: Bool 
  , jumpCount :: Int
  } deriving (Eq, Show)

data Platform = Platform
  { platformX :: Float
  , platformY :: Float
  , platformWidth :: Float
  , platformHeight :: Float
  , platformImage :: Picture
  } deriving (Eq, Show)

data Coin = Coin
  { coinX :: Float
  , coinY :: Float
  , coinImage :: Picture
  } deriving (Eq, Show)

backgroundPath :: FilePath
backgroundPath = "./background.bmp" 

playerPathRight :: FilePath
playerPathRight = "./gracz_prawo.bmp"

playerPathLeft :: FilePath
playerPathLeft = "./gracz_lewo.bmp"

platformPath :: FilePath
platformPath = "./platforma.bmp"

coinPath :: FilePath
coinPath = "./moneta.bmp"

highScoresFilePath :: String
highScoresFilePath = "high_scores.txt"

fps :: Int
fps = 60

gravity :: Float
gravity = -1000