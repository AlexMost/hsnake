module Primitives(
    Coord(..), up, down, left, right,
    Stage(..), Direction(..), Snake(..), GameState(..), Controls(..),
    GameStatus(..), snakeMove
    ) where


data Direction = LEFT | RIGHT | UP | DOWN
    deriving (Show)


data Controls = QUIT | RESTART | STOP
    deriving (Show)


data Coord = Coord {x :: Int, y :: Int} deriving (Show)


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


up :: Coord -> Coord
up (Coord x y) = Coord x (y + 1)


down :: Coord -> Coord
down (Coord x y) = Coord x (y - 1)


left :: Coord -> Coord
left (Coord x y) = Coord (x -1) y


right :: Coord -> Coord
right (Coord x y) = Coord (x + 1) y