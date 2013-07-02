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
display (Just x) = putStrLn $ "you have entered -- " ++ [x]
display Nothing = return ()


gameStart :: IO ()
gameStart = do 
    threadDelay 20000
    result <- hReady stdin >>= keyListen
    display result
    gameStart

    
