module Main (main) where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import RunGame
import CoinData
import BasicData
import System.Exit (exitSuccess)


main :: IO ()
main = playIO (InWindow "Menu" (600, 400) (100, 100)) black 60 initialMenuState drawMenu handleMenuKeys (\_ menuState -> return menuState)
