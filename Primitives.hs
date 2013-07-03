module Primitives(
    Coord(..), up, down, left, right,
    Stage(..), Direction(..), Snake(..), GameState(..), Controls(..),
    GameStatus(..), snakeMove, stageBorders
    ) where


data Direction = LEFT | RIGHT | UP | DOWN
    deriving (Show)


data Controls = QUIT | RESTART | STOP
    deriving (Show)


data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)


data Stage = Stage {  width :: Int
                    , height :: Int
                    , offset :: Coord
                    }
    deriving(Show)


data Snake = Snake {cords :: [Coord]} deriving(Show)


data GameState = GameState  { direction :: Direction
                            , stage     :: Stage
                            , snake     :: Snake
                            , score     :: Int
                            , apple     :: Coord
                            }


data GameStatus = Continue | Loose | Win | Quit


snakeMove :: Direction -> Snake -> Snake
snakeMove direction oldS@Snake{cords=cords} =
    Snake (newCord: init cords)
        where 
            newCord = action $ head cords
            action = case direction of
                UP -> up
                DOWN -> down
                LEFT -> left
                RIGHT -> right


stageBorders :: Stage -> [Coord]
stageBorders st@Stage{width=w, height=h, offset=o} =
    foldl (++) [] [head_line, bottom_line, left_line, right_line]
    where
        head_line = [Coord i 0 | i <- [0..w]]
        bottom_line = [Coord i h | i <- [0..w]]
        left_line = [Coord 0 j | j <- [0..h]]
        right_line = [Coord w j | j <- [0..h]]


up :: Coord -> Coord
up (Coord x y) = Coord (x -1) y


down :: Coord -> Coord
down (Coord x y) = Coord (x + 1) y


left :: Coord -> Coord
left (Coord x y) = Coord x (y -1)


right :: Coord -> Coord
right (Coord x y) = Coord x (y + 1)