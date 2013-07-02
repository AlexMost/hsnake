import UI.HSCurses.Curses
import System.Exit
import System.IO
import Control.Monad
import Primitives
import Draw
import Game


main :: IO ()
main = do
  mainWin <- initScr
  hSetBuffering stdin NoBuffering
  initCurses
  echo False
  wclear stdScr
  gameStart
