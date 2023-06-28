module Main (main) where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import RunGame
import CoinData
import BasicData
import System.Exit (exitSuccess)

data MenuOption = Play | Quit deriving (Eq)

data MenuState = MenuState
  { selectedOption :: MenuOption
  , menuOptions :: [MenuOption]
  }

initialMenuState :: MenuState
initialMenuState = MenuState { selectedOption = Play, menuOptions = [Play, Quit] }

drawMenu :: MenuState -> IO Picture
drawMenu menuState = return $ pictures $ zipWith (drawOption (selectedOption menuState)) [1 , (-1)] (menuOptions menuState)
  where
    drawOption :: MenuOption -> Int -> MenuOption -> Picture
    drawOption selectedOption index option =
      let optionText = case option of
            Play -> "Play"
            Quit -> "Quit"
          colorFunc = if selectedOption == option then color green else color white
      in translate (-125) (fromIntegral index * (75)) $ colorFunc $ text optionText

handleMenuKeys :: Event -> MenuState -> IO MenuState
handleMenuKeys (EventKey (SpecialKey KeyUp) Down _ _) menuState =
  let options = menuOptions menuState
      currentOption = selectedOption menuState
      newOption = case currentOption of
        Play -> last options
        Quit -> options !! (length options - 2)
  in return $ menuState { selectedOption = newOption }
handleMenuKeys (EventKey (SpecialKey KeyDown) Down _ _) menuState =
  let options = menuOptions menuState
      currentOption = selectedOption menuState
      newOption = case currentOption of
        Play -> options !! 1
        Quit -> head options
  in return $ menuState { selectedOption = newOption }
handleMenuKeys (EventKey (SpecialKey KeyEnter) Down _ _) menuState =
  let selected = selectedOption menuState
  in case selected of
    Play -> do
      putStrLn "Starting the game..."
      startGame
      return menuState
    Quit -> do
      putStrLn "Quitting the game..."
      exitSuccess
      return menuState
handleMenuKeys _ menuState = return menuState

startGame :: IO ()
startGame = do
  putStrLn "Rozpoczynamy grę!"
  initialState <- initialState
  playGame 0.0 initialState


playGame :: Float -> GameState -> IO ()
playGame dt gameState = do
    playIO (InWindow "Game" (windowWidth, windowHeight) (0, 0))
         black
         fps
         gameState
         draw
         handleKeys
         update
    if (gameOver gameState)
      then do
        putStrLn "Przegrałeś!"
        playAgain <- askToPlayAgain
        if playAgain
          then do
            putStrLn "Nowa gra rozpoczyna się..."
            initialState <- initialState
            playGame dt initialState
          else do
            putStrLn "Dziękujemy za grę. Do widzenia!"
      else do
      updatedGameState <- update dt gameState
      playGame dt updatedGameState


main :: IO ()
main = playIO (InWindow "Menu" (600, 400) (100, 100)) black 60 initialMenuState drawMenu handleMenuKeys (\_ menuState -> return menuState)