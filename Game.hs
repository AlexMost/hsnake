module Game(start) where 
import Primitives

data GameState = Game {  direction :: Direction
                       , stage :: Stage
                       , snake :: Snake
                       }

start :: IO ()
start = 