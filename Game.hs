module Game(gameStart, gameLoop, GameState(..)) where 

import Control.Applicative
import System.IO
import Control.Concurrent(threadDelay)
import Primitives
import UI.HSCurses.Curses


data GameState = GameState  { direction :: Direction
                            , stage     :: Stage
                            , snake     :: Snake
                            }


keyListen :: IO (Maybe Char)
keyListen = do
    result <- hReady stdin
    case result of
        True -> (Just) <$> getChar
        False -> return Nothing


getNewDir :: Direction -> IO Direction
getNewDir old = do
    key <- keyListen
    case key of
        (Just 'w') -> return UP
        (Just 's') -> return DOWN
        (Just 'a') -> return LEFT
        (Just 'd') -> return RIGHT
        _ -> return old


gameStart :: GameState -> IO (Either String GameState)
gameStart gs@GameState{direction=direction, stage=stage, snake=snake} = 
    do 
        threadDelay 200000
        newDir <- getNewDir direction
        putStrLn $ show newDir
        case newDir of
            DOWN -> return $ Left "Down is pressed"
            _ -> return $ Right(GameState newDir stage snake)
    

gameLoop :: GameState -> IO String
gameLoop gs = gameStart gs >>= either return gameLoop

    
