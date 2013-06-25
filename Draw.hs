module Draw(Draw, write) where 

import UI.HSCurses.Curses(mvWAddStr, stdScr, refresh)
import Primitives

write :: String -> Coord -> IO ()
write str (Coord x y) = mvWAddStr stdScr x y str

class Draw a where
    draw :: a -> IO ()

instance Draw Coord where
    draw co = write "#"

