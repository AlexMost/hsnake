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
    draw co = write "*" co


instance Draw Stage where
    draw stage = writePoints (stageBorders stage)


instance Draw GameState where
    draw GameState{stage=stage, snake=snake, apple=apple} = do
        draw stage >> draw snake >> draw apple


instance Draw Snake where
    draw Snake{cords=cords} = writePoints cords