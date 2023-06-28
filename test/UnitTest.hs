import Test.HUnit
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import BasicData
import CoinData
import RunGame

testGameStateCreation :: Test
testGameStateCreation = TestCase $ do
  let playerState = PlayerState 0 0 0 0 False (blank, blank) True 0
      platforms = []
      coins = []
      background = blank
      score = 0
      level = 0
      gameOver = False
      gameState = GameState playerState platforms coins background score level gameOver

  assertEqual "GameState creation" gameState (GameState playerState platforms coins background score level gameOver)

testPlayerStateCreation :: Test
testPlayerStateCreation = TestCase $ do
  let playerState = PlayerState 0 0 0 0 False (blank, blank) True 0

  assertEqual "PlayerState creation" playerState (PlayerState 0 0 0 0 False (blank, blank) True 0)

testPlatformCreation :: Test
testPlatformCreation = TestCase $ do
  let platform = Platform 0 0 100 20 blank

  assertEqual "Platform creation" platform (Platform 0 0 100 20 blank)


testCoinCreation :: Test
testCoinCreation = TestCase $ do
  let coin = Coin 0 0 blank

  assertEqual "Coin creation" coin (Coin 0 0 blank)

testConstants :: Test
testConstants = TestCase $ do
  assertEqual "Window width" 1200 windowWidth
  assertEqual "Window height" 800 windowHeight
  assertEqual "Background path" "./background.bmp" backgroundPath
  assertEqual "Player path (right)" "./gracz_prawo.bmp" playerPathRight
  assertEqual "Player path (left)" "./gracz_lewo.bmp" playerPathLeft
  assertEqual "Platform path" "./platforma.bmp" platformPath
  assertEqual "Coin path" "./moneta.bmp" coinPath


testDrawCoins :: Test
testDrawCoins = TestCase $ do
  let coin1 = Coin 10 10 (rectangleSolid 10 10)
      coin2 = Coin 20 20 (rectangleSolid 10 10)
      coins = [coin1, coin2]
  coinPictures <- drawCoins coins
  assertEqual "Coin pictures count" 2 (length coinPictures)

testStartJump :: Test
testStartJump = TestCase $ do
  let initialState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      expectedState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = True, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      actualState = startJump initialState

  assertEqual "Start jump" expectedState actualState

testStopJump :: Test
testStopJump = TestCase $ do
  let initialState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = True, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      expectedState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      actualState = stopJump initialState

  assertEqual "Stop jump" expectedState actualState

testStopMovingLeft :: Test
testStopMovingLeft = TestCase $ do
  let initialState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = -150, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      expectedState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      actualState = stopMovingLeft initialState

  assertEqual "Stop moving left" expectedState actualState

testStopMovingRight :: Test
testStopMovingRight = TestCase $ do
  let initialState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 150, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      expectedState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      actualState = stopMovingRight initialState

  assertEqual "Stop moving right" expectedState actualState

testMoveLeft :: Test
testMoveLeft = TestCase $ do
  let initialState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      expectedState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = -150, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = False, jumpCount = 0 }
      actualState = moveLeft initialState

  assertEqual "Move left" expectedState actualState

testMoveRight :: Test
testMoveRight = TestCase $ do
  let initialState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 0, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      expectedState = PlayerState { playerX = 0, playerY = 0, playerXSpeed = 150, playerYSpeed = 0, isJumping = False, playerImages = (Blank, Blank), isFacingRight = True, jumpCount = 0 }
      actualState = moveRight initialState

  assertEqual "Move right" expectedState actualState


testSuite :: Test
testSuite = TestList
  [ TestLabel "testDrawCoins" testDrawCoins
  , TestLabel "testGameStateCreation" testGameStateCreation
  , TestLabel "testPlayerStateCreation" testPlayerStateCreation
  , TestLabel "testPlatformCreation" testPlatformCreation
  , TestLabel "testCoinCreation" testCoinCreation
  , TestLabel "testConstants" testConstants
  , testStopJump
  , testStopMovingLeft
  , testStopMovingRight
  , testMoveLeft
  , testMoveRight
  ]

-- Run the test suite
main :: IO Counts
main = runTestTT testSuite