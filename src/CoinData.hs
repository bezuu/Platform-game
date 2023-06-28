module CoinData where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import BasicData

generateCoinsForLevel :: Int -> [Platform] -> IO [Coin]
generateCoinsForLevel level platforms = do
  let numCoins = level + 1 
  initialCoins numCoins platforms

initialCoins :: Int -> [Platform] -> IO [Coin]
initialCoins count platforms = do
  gen <- newStdGen
  let coinCoords = randomCoords gen count platforms
  mapM (\(x, y) -> generateCoin x y) coinCoords

generateCoin :: Float -> Float -> IO Coin
generateCoin x y = do
  coinImage <- loadBMP coinPath
  return (Coin x y coinImage)

randomFloat :: Float -> Float -> IO Float
randomFloat min max = getStdRandom (randomR (min, max))

randomCoords :: StdGen -> Int -> [Platform] -> [(Float, Float)]
randomCoords gen count platforms = foldr fixCoord [] initialCoords
  where
    fixCoord :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
    fixCoord coord acc
      | isTooClose coord platforms = moveCoord coord : acc
      | otherwise = coord : acc

    moveCoord :: (Float, Float) -> (Float, Float)
    moveCoord (x, y) = (x + offsetX, y + offsetY)
      where
        offsetX = if collisionFromLeft x platforms then -35 else if collisionFromRight x platforms then 35 else 0
        offsetY = if collisionTop y platforms then 70 else if collisionFromTop y platforms then 35 else if collisionFromBottom y platforms then -35 else 0

    collisionFromLeft :: Float -> [Platform] -> Bool
    collisionFromLeft x = any (\(Platform px _ _ _ _) -> x + 30 > px)

    collisionFromRight :: Float -> [Platform] -> Bool
    collisionFromRight x = any (\(Platform px _ pw _ _) -> x - 30 < px + pw)

    collisionFromBottom :: Float -> [Platform] -> Bool
    collisionFromBottom y = any (\(Platform _ py _ _ _) -> (y + 35 > py && y < py))

    collisionFromTop :: Float -> [Platform] -> Bool
    collisionFromTop y = any (\(Platform _ py _ ph _) -> (y - 35 < py + ph && y > py + ph ))

    collisionTop :: Float -> [Platform] -> Bool
    collisionTop y = any (\(Platform _ py _ ph _) -> (y < py + ph && y > py ))

    isTooClose :: (Float, Float) -> [Platform] -> Bool
    isTooClose (x, y) = any (\(Platform px py pw ph _) -> isCollision x y px py pw ph)

    isCollision :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
    isCollision x y px py pw ph =
      let right = px + pw
          top = py + ph
      in (x + 30 > px && x - 30 < right) || (y + 35 > py && y - 35 < top)

    initialCoords :: [(Float, Float)]
    initialCoords = take count $ zip xs ys

    (genX, genY) = split gen
    xs = take count $ randomRs (-540, 540) genX
    ys = take count $ randomRs (-325, 325) genY

updateCoins :: Float -> GameState -> GameState
updateCoins dt gameState =
  let player = playerState gameState
      collidedCoins = filter (collidingWithPlayer player) (coins gameState)
      remainingCoins = filter (not . collidingWithPlayer player) (coins gameState)
      updatedScore = score gameState + length collidedCoins
   in gameState { coins = remainingCoins, score = updatedScore }
  where
    collidingWithPlayer player coin = distance (playerX player, playerY player) (coinX coin, coinY coin) <= 60
    distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

drawCoins :: [Coin] -> IO [Picture]
drawCoins coins = do
  coinImages <- mapM (\c -> return (coinImage c)) coins
  let coinPictures = zipWith (\(Coin x y _) coinImage -> translate x y $ scale 0.034 0.034 $ coinImage) coins coinImages
  return coinPictures