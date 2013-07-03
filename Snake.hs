import UI.HSCurses.Curses
import System.Exit
import System.IO
import Control.Monad
import Primitives
import Draw
import Game


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  initCurses
  echo False
  msg <- gameLoop $ GameState direction stage snake 0 apple
  endWin
  putStrLn msg
  where
    stage = Stage 20 40 (Coord 0 0)
    direction = RIGHT
    snake = Snake [(Coord 2 2)]
    apple = Coord 4 4
