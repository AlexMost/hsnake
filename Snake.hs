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
  initCurses
  echo False
  wclear mainWin
  refresh
  msg <- gameLoop $ GameState direction stage snake
  endWin
  putStrLn msg
  where
    stage = Stage 20 10 (Coord 0 0)
    direction = RIGHT
    snake = Snake [(Coord 2 2)]
