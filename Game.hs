module Game(gameStart, gameLoop) where

import Control.Applicative
import System.IO
import System.Random
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


getNewGameState :: GameState -> GameStatus -> Direction -> Coord -> GameState
getNewGameState 
    gs@GameState{snake=snake, stage=stage, apple=apple, score=score} 
    status 
    dir
    newApple
    | status == Continue = GameState dir stage newSnake score apple
    | status == HitApple = GameState dir stage biggerSnake (score+1) newApple
    where newSnake = snakeMove dir snake
          biggerSnake = Snake (apple:cords snake)


getGameStatus :: GameState -> Direction -> GameStatus
getGameStatus 
    gs@GameState{direction=dir, snake=snake, stage=stage, apple=apple} 
    newDir
    | nextHeadPosition `elem` stBorders       = Loose
    | nextHeadPosition `elem` cords(snake)    = Loose
    | nextHeadPosition == apple               = HitApple
    | otherwise                               = Continue
    where 
        nextHeadPosition = head $ cords(snakeMove newDir snake)
        stBorders = stageBorders stage


getApplePosition :: Stage -> IO Coord
getApplePosition st@Stage{width=w, height=h} =
    do
        x <- randomRIO (1, w-1)
        y <- randomRIO (1, h-1)
        return $ Coord x y


gameStart :: GameState -> IO (Either String GameState)
gameStart gs@GameState{direction=direction, score=score, snake=snake, apple=apple} =
    do
        wclear stdScr
        draw gs
        refresh
        threadDelay 200000
        key <- keyListen
        newApple <- getApplePosition $ stage gs
        let newDir = getNewDir direction key
        let gameStatus = getGameStatus gs newDir
        case gameStatus of
            Continue -> return $ Right(getNewGameState gs Continue newDir newApple)
            HitApple -> return $ Right(getNewGameState gs HitApple newDir newApple)
            Loose -> return $ Left("You looose your score is " ++ show score)


gameLoop :: GameState -> IO String
gameLoop gs = gameStart gs >>= either return gameLoop