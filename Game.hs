module Game(gameStart, gameLoop) where

import Control.Applicative
import System.IO
import UI.HSCurses.Curses
import Control.Concurrent(threadDelay)
import Primitives
import Draw


keyListen :: IO (Maybe Char)
keyListen = do
    result <- hReady stdin
    if result 
        then (Just) <$> getChar 
        else return Nothing


getNewDir :: Direction -> Maybe Char -> Direction
getNewDir DOWN (Just 'w') = DOWN
getNewDir _ (Just 'w') = UP
getNewDir UP (Just 's') = UP
getNewDir _ (Just 's') = DOWN
getNewDir RIGHT (Just 'a') = RIGHT
getNewDir _ (Just 'a') = LEFT
getNewDir LEFT (Just 'd') = LEFT
getNewDir _ (Just 'd') = RIGHT
getNewDir old Nothing = old


getNewGameState :: GameState -> Direction -> GameState
getNewGameState gs@GameState{snake=snake, stage=stage, apple=apple, score=score} dir =
    GameState dir stage newSnake score apple
        where newSnake = snakeMove dir snake


getGameStatus :: GameState -> Direction -> GameStatus
getGameStatus gs@GameState{direction=dir, snake=snake, stage=stage} newDir = 
    Continue


gameStart :: GameState -> IO (Either String GameState)
gameStart gs@GameState{direction=direction, score=score, snake=snake, apple=apple} =
    do
        wclear stdScr
        draw gs
        refresh
        threadDelay 200000
        key <- keyListen
        let newDir = getNewDir direction key
        let gameStatus = getGameStatus gs newDir
        case gameStatus of
            Continue -> return $ Right(getNewGameState gs newDir)
            Loose -> return $ Left("You looose your score is " ++ show score)
            Quit -> return $ Left "Buy - buy"
            Win -> return $ Left "Great job dude !!!"


gameLoop :: GameState -> IO String
gameLoop gs = gameStart gs >>= either return gameLoop
