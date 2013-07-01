import UI.HSCurses.Curses
import System.Exit
import Control.Monad
import Primitives
import Draw
import Game


main :: IO ()
main = do
  initCurses
  echo False
  gameStart
