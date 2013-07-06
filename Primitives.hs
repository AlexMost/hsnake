module Primitives(
    Coord(..), Stage(..), Direction(..), Snake(..), GameState(..),
    GameStatus(..), snakeMove, stageBorders
    ) where


data Direction = LEFT | RIGHT | UP | DOWN
    deriving (Show)


data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)


data Stage = Stage {  width :: Int
                    , height :: Int
                    , offset :: Coord
                    }
    deriving(Show)


newtype Snake = Snake {cords :: [Coord]} deriving(Show, Eq)


data GameState = GameState  { direction :: Direction
                            , stage     :: Stage
                            , snake     :: Snake
                            , score     :: Int
                            , apple     :: Coord
                            }


data GameStatus = Continue | Loose | Win | Quit | HitApple
    deriving(Show, Eq)


snakeMove :: Direction -> Snake -> Snake
snakeMove direction oldS@Snake{cords=cords} =
    Snake (newCord: init cords)
        where 
            newCord = action $ head cords
            action = case direction of
                UP -> \Coord{x=x, y=y} -> Coord (x - 1) y
                DOWN -> \Coord{x=x, y=y} -> Coord (x + 1) y
                LEFT -> \Coord{x=x, y=y} -> Coord x (y -1)
                RIGHT -> \Coord{x=x, y=y} -> Coord x (y + 1)


stageBorders :: Stage -> [Coord]
stageBorders st@Stage{width=w, height=h, offset=o} =
    foldl (++) [] [head_line, bottom_line, left_line, right_line]
    where
        head_line = [Coord i 0 | i <- [0..w]]
        bottom_line = [Coord i h | i <- [0..w]]
        left_line = [Coord 0 j | j <- [0..h]]
        right_line = [Coord w j | j <- [0..h]]

