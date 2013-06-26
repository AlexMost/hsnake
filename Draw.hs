module Draw(
    Draw(), draw, write, writePoint
    ) where 

import UI.HSCurses.Curses(mvWAddStr, stdScr, refresh)
import Primitives

write :: String -> Coord -> IO ()
write str (Coord x y) = mvWAddStr stdScr x y str

writePoint :: Coord -> IO ()
writePoint co = write "*" co

writePoints :: [Coord] -> IO ()
writePoints cords = mapM_ writePoint cords

class Draw a where
    draw :: a -> IO ()

instance Draw Coord where
    draw co = write "#" co

instance Draw Stage where
    draw Stage {width = w, height = h, offset = o} = do
        mapM_ writePoints [head_line, bottom_line, left_line, right_line]
        where
            head_line = [Coord i 0 | i <- [0..w]]
            bottom_line = [Coord i h | i <- [0..w]]
            left_line = [Coord 0 j | j <- [0..h]]
            right_line = [Coord w j | j <- [0..h]]


