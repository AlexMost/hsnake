module Primitives(
    Coord, up, down, left, right, Stage
    ) where

data Coord = Coord {x::Int, y::Int} deriving (Show)

data Stage = Stage {width::Int, height::Int, offset::Coord}
    deriving(Show)

up :: Coord -> Coord
up (Coord x y) = Coord x (y + 1)

down :: Coord -> Coord
down (Coord x y) = Coord x (y - 1)

left :: Coord -> Coord
left (Coord x y) = Coord (x -1) y

right :: Coord -> Coord
right (Coord x y) = Coord (x + 1) y