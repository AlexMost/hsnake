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
  msg <- gameLoop $ GameState direction stage snake 0 apple
  endWin
  putStrLn msg
  where
    stage = Stage 10 20 (Coord 0 0)
    direction = RIGHT
    snake = Snake [(Coord 2 2)]
    apple = Coord 4 4
