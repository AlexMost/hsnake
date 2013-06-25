import UI.HSCurses.Curses
import Primitives
import Draw


main :: IO ()
main = do
  mainWin <- initScr
  initCurses
  echo False
  a = Coord 5 5
  draw a
  refresh
  return ()
