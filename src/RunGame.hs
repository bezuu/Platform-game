module RunGame where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import CoinData
import BasicData
import System.Exit (exitSuccess)
import System.IO
import System.FilePath
import System.Directory
import Data.Maybe
import Data.List (sortBy)
import Data.List
import Text.Read
import Control.Monad

initialState :: IO GameState
initialState = do
  background <- loadBMP backgroundPath
  playerImageRight <- loadBMP playerPathRight 
  playerImageLeft <- loadBMP playerPathLeft
  platformImage <- loadBMP platformPath
  initialCoinList <- initialCoins 3 (initialPlatforms platformImage)
  return GameState
    { playerState = initialPlayerState (playerImageLeft, playerImageRight)
    , platforms = initialPlatforms platformImage
    , coins = initialCoinList
    , background = background
    , score = 0
    , level = 1
    , gameOver = False
    }

initialPlayerState :: (Picture, Picture) -> PlayerState
initialPlayerState playerImages = PlayerState
  { playerX = -380
  , playerY = -220
  , playerXSpeed = 0
  , playerYSpeed = 0
  , isJumping = False
  , playerImages = playerImages
  , isFacingRight = True
  , jumpCount = 0
  }

initialPlatforms :: Picture -> [Platform]
initialPlatforms platformImage = 
    [ Platform (-430) (-325) 800 30 platformImage
    , Platform 350 (-50) 150 30 platformImage
    , Platform (-500) (-50) 250 30 platformImage
    , Platform (-150) (-150) 400 30 platformImage
    , Platform 250 200 135 30 platformImage
    , Platform (-250) 125 325 30 platformImage
    ]

drawPlatforms :: [Platform] -> IO [Picture]
drawPlatforms platforms = do
  platformImages <- mapM (\(Platform _ _ _ _ imagePath) -> return imagePath) platforms
  let platformPictures = zipWith (\platform image -> translate (platformX platform + platformWidth platform / 2) (platformY platform + platformHeight platform / 2) $ scale (platformWidth platform / 523) (platformHeight platform / 173)image) platforms platformImages
  return platformPictures

draw :: GameState -> IO Picture
draw (GameState playerState platforms coins background score level gameover) = do
  coinPictures <- drawCoins coins
  platformPictures <- drawPlatforms platforms 
  let scoreText = translate (-(fromIntegral windowWidth / 2 - 10)) (fromIntegral windowHeight / 2 - 30) $ scale 0.2 0.2 $ color black $ text ("Score: " ++ show score)
      levelText = translate (-(fromIntegral windowWidth / 2 - 10)) (fromIntegral windowHeight / 2 - 55)  $ scale 0.2 0.2 $ color black $ text ("Level: " ++ show level)
      playerXPos = playerX playerState
      playerYPos = playerY playerState
      playerImage = if isFacingRight playerState then snd (playerImages playerState) else fst (playerImages playerState)
      playerPicture = translate playerXPos playerYPos playerImage
  return $ pictures (background : playerPicture : platformPictures ++ coinPictures ++ [scoreText] ++ [levelText])

update :: Float -> GameState -> IO GameState
update dt gameState = do
  let gameState' = updatePlayer dt gameState
      gameState'' = updateCoins dt gameState'
  if gameOver gameState''
    then do
      let finalScore = score gameState''
      putStrLn $ "Przegrałeś! Twój wynik: " ++ show finalScore
      updateHighScores highScoresFilePath finalScore
      return gameState''
      playAgain <- askToPlayAgain
      if playAgain
        then do
          putStrLn "Nowa gra rozpoczyna się..."
          initialGameState <- initialState
          return initialGameState
        else do
          putStrLn "Dziękujemy za grę. Do widzenia!"
          exitSuccess
    else if null (coins gameState'')
      then do
        let nextLevel = level gameState'' + 1
        newCoins <- generateCoinsForLevel (nextLevel + 1) (platforms gameState'')
        playerImageRight <- loadBMP playerPathRight 
        playerImageLeft <- loadBMP playerPathLeft
        return gameState'' { coins = newCoins, level = nextLevel, score = score gameState'', playerState = initialPlayerState (playerImageLeft, playerImageRight)}
      else return gameState''
gameover :: GameState -> Bool
gameover gamestate = 
  if playerY (playerState gamestate) < (-(fromIntegral windowHeight / 2) - 100) then True else False

updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gameState =
  if isJumping (playerState gameState)
    then gameState { playerState = updateJumpingPlayer dt (platforms gameState) (playerState gameState) , gameOver = gameover gameState}
    else gameState { playerState = updateFallingPlayer dt (platforms gameState) (playerState gameState) , gameOver = gameover gameState}

updateJumpingPlayer :: Float -> [Platform] -> PlayerState -> PlayerState
updateJumpingPlayer dt platforms playerState =
  let newY = playerY playerState + playerYSpeed playerState * dt
      limitedY = if (fromIntegral windowHeight / 2 - 45) < newY then (fromIntegral windowHeight / 2 - 45) else newY
      newX = playerX playerState + playerXSpeed playerState * dt
      limitedX = if (fromIntegral windowWidth / 2 - 30) < newX then (fromIntegral windowWidth / 2 - 30) else if  (fromIntegral (-windowWidth) / 2 + 30) > newX 
        then (fromIntegral (-windowWidth) / 2 + 30) else newX 
      newSpeed = if (fromIntegral windowHeight / 2 - 45) < newY then (-playerYSpeed playerState) / 2 + gravity * dt else playerYSpeed playerState + gravity * dt
      newYSpeed = if jumpCount playerState >= 3 then -350 else newSpeed
   in checkCollision platforms playerState { playerY = limitedY, playerYSpeed = newYSpeed , playerX = limitedX}

updateFallingPlayer :: Float -> [Platform] -> PlayerState -> PlayerState
updateFallingPlayer dt platforms playerState =
  let newY = playerY playerState + playerYSpeed playerState * dt
      limitedY = if (fromIntegral windowHeight / 2 - 45) < newY then (fromIntegral windowHeight / 2 - 45) else newY
      newX = playerX playerState + playerXSpeed playerState * dt
      limitedX = if (fromIntegral windowWidth / 2 - 30) < newX then (fromIntegral windowWidth / 2 - 30) else if  (fromIntegral (-windowWidth) / 2 + 30) > newX 
        then (fromIntegral (-windowWidth) / 2 + 30) else newX 
      newSpeed = if (fromIntegral windowHeight / 2 - 45) < newY then (-playerYSpeed playerState) / 2 + gravity * dt else playerYSpeed playerState + gravity * dt
      newYSpeed = if jumpCount playerState >= 3 then -350 else newSpeed
   in checkCollision platforms playerState { playerY = limitedY, playerYSpeed = newYSpeed , playerX = limitedX}


checkCollision :: [Platform] -> PlayerState -> PlayerState
checkCollision platforms playerState =
  let (PlayerState px py pxSpeed pySpeed jumping _ _ _) = playerState
      onPlatform = filter (\platform -> collidingWithPlatform platform playerState) platforms
   in if not (null onPlatform)
        then if py <= platformY (head onPlatform) && (px + 25 >= platformX (head onPlatform) && px - 25 <= platformX (head onPlatform) + platformWidth (head onPlatform))
               then playerState {playerYSpeed = -250} -- odbijanie się z dołu 
               else if (py >= platformY (head onPlatform) && py - 40 <= platformY (head onPlatform) + platformHeight (head onPlatform)) && (px + 25 >= platformX (head onPlatform) && px - 25 <= platformX (head onPlatform) + platformWidth (head onPlatform))
                then playerState { playerYSpeed = -250, playerXSpeed = 0}  -- niewskoczenie na platforme
                  else if (px + 20 < platformX (head onPlatform) || px - 20 > platformX (head onPlatform) + platformWidth (head onPlatform)) 
                    then playerState {playerXSpeed = 0}
                else playerState { playerYSpeed = 0 , jumpCount = 0} -- utrzymanie 
      else playerState

collidingWithPlatform :: Platform -> PlayerState -> Bool
collidingWithPlatform platform playerState =
  let (Platform px py pw ph _) = platform
      (PlayerState px' py' _ _ _ _ _ _) = playerState
   in px' + 20 >= px
        && px' - 20 <= px + pw
        && py' + 50 >= py 
        && py' - 50 <= py + ph  

handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState =
  if not (isJumping (playerState gameState))
    then return (gameState { playerState = startJump (playerState gameState) })
    else return gameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState =
  return (gameState { playerState = moveLeft (playerState gameState) })
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState =
  return (gameState { playerState = moveRight (playerState gameState) })
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) gameState =
  return (gameState { playerState = stopJump (playerState gameState) })
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) gameState =
  return (gameState { playerState = stopMovingLeft (playerState gameState) })
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) gameState =
  return (gameState { playerState = stopMovingRight (playerState gameState) })
handleKeys _ gameState = return gameState

startJump :: PlayerState -> PlayerState
startJump playerState =
  playerState { playerYSpeed = 500, isJumping = True, jumpCount = jumpCount playerState + 1 }

stopJump :: PlayerState -> PlayerState
stopJump playerState =
  playerState { isJumping = False }

stopMovingLeft :: PlayerState -> PlayerState
stopMovingLeft playerState =
  if playerXSpeed playerState < 0 then playerState { playerXSpeed = 0 } else playerState

stopMovingRight :: PlayerState -> PlayerState
stopMovingRight playerState =
  if playerXSpeed playerState > 0 then playerState { playerXSpeed = 0 } else playerState

moveLeft :: PlayerState -> PlayerState
moveLeft playerState =
  playerState { playerXSpeed = (-150), isFacingRight = False }

moveRight :: PlayerState -> PlayerState
moveRight playerState =
  playerState { playerXSpeed = 150, isFacingRight = True }



-----------------------------

createTextFile :: FilePath -> IO ()
createTextFile path = do
  removeIfExists path
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode $ \handle ->
    hClose handle

compareAndUpdateLines :: FilePath -> FilePath -> IO ()
compareAndUpdateLines path path2 = do
  exists <- doesFileExist path2
  if exists
    then do
      sessionRecord <- readFirstLineAsInt path
      personalRecord <- readFirstLineAsInt path2
      let updatedLine = max sessionRecord personalRecord
      writeFirstLine path2 updatedLine
    else do
      createTextFile path2
      firstLine <- readFirstLineAsInt path
      writeFirstLine path2 firstLine

readFirstLineAsInt :: FilePath -> IO Int
readFirstLineAsInt path = do
  withFile path ReadMode $ \handle -> do
    contents <- hGetLine handle
    case readMaybe contents of
      Just value -> return value
      Nothing -> error "Invalid format in file"

writeFirstLine :: FilePath -> Int -> IO ()
writeFirstLine path value = withFile path AppendMode $ \handle -> do
  hPrint handle value

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

updateHighScores :: FilePath -> Int -> IO ()
updateHighScores path newScore = do
  let tempPath = path ++ ".tmp"
  createTextFile tempPath
  scores <- readHighScores path

  let updatedScores = insert newScore scores
      sortedScores = sortBy (flip compare) updatedScores

  writeHighScores tempPath sortedScores
  renameFile tempPath path

readHighScores :: FilePath -> IO [Int]
readHighScores path = do
  contents <- readFile path
  let scores = mapMaybe parseScore (lines contents)
  return scores

parseScore :: String -> Maybe Int
parseScore str = readMaybe str

writeHighScores :: FilePath -> [Int] -> IO ()
writeHighScores path scores = do
  let formattedScores = map show scores
      content = unlines formattedScores
  writeFile path content

askToPlayAgain :: IO Bool
askToPlayAgain = do
  putStrLn "Czy chcesz zagrać ponownie? (t/n)"
  response <- getLine
  case response of
    "t" -> return True
    "n" -> return False
    _   -> do
      putStrLn "Nieprawidłowa odpowiedź. Spróbuj jeszcze raz."
      askToPlayAgain