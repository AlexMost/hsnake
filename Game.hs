module Game(gameStart) where 
import Control.Applicative
import System.IO
import Control.Concurrent(threadDelay)
import Primitives

data GameState = GameState {  direction :: Direction
                            , stage :: Stage
                            , snake :: Snake
                       }

keyListen :: Bool -> IO (Maybe Char)
keyListen True = (Just) <$> getChar
keyListen False = return Nothing

display :: Maybe Char -> IO ()
display (Just x) = putStrLn $ "you entered -- " ++ [x]
display Nothing = return ()

gameStart :: IO ()
gameStart = do 
    threadDelay 20000
    ready <- hReady stdin
    result <- keyListen ready
    display result
    gameStart

    
