import UI.HSCurses.Curses
import System.Exit
import Control.Monad
import Primitives
import Draw


main :: IO ()
main = do
  mainWin <- initScr
  initCurses
  echo False
  let stage = Stage 10 10 (Coord 1 1)
  draw stage
  refresh
