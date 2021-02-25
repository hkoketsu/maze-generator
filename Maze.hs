{-# LANGUAGE FlexibleContexts #-}
import System.Random ( randomRIO )
import Data.STRef ()
import Data.Array.IO
    ( readArray, writeArray, MArray(newArray), IOArray )
import Data.Array ()
import Data.Array.MArray ()
import Control.Monad ( unless, forM_ )
import Control.Monad.ST ()


type Maze = (
        (Int, Int),
        (Int, Int), 
        (Int, Int), 
        IOArray (Int, Int) Bool, 
        IOArray (Int, Int) Bool
    )


createMaze :: Int -> Int -> IO Maze
createMaze width height = do
    rightWalls  <- mazeGrid True
    bottomWalls <- mazeGrid True
    visited     <- mazeGrid False

    start <- getStartPoint width height

    createMazeByDfs rightWalls bottomWalls visited start

    let goal = (width-1 - (fst start), height-1 - (snd start))

    return ((width, height), start, goal, rightWalls, bottomWalls)

    where
        createMazeByDfs rightWalls bottomWalls visited here = do
            writeArray visited here True
            let ns = neighbours here width height
            i <- randomRIO (0, length ns - 1)
            forM_ (ns !! i : take i ns ++ drop (i + 1) ns) $ \next -> do
                let x1 = fst here
                let y1 = snd here
                let x2 = fst next
                let y2 = snd next
                nextVisited <- readArray visited next
                unless nextVisited $
                  writeArray (if x1 == x2 then bottomWalls else rightWalls) (min x1 x2, min y1 y2) False
                unless nextVisited $
                  createMazeByDfs rightWalls bottomWalls visited next
        
        mazeGrid = newArray ((0, 0), (width-1, height-1)) :: Bool -> IO (IOArray (Int, Int) Bool)



printMaze :: Maze -> IO ()
printMaze ((width, height), start, goal, rightWalls, bottomWalls) = do
  putStrLn $ '+' : concat (replicate width "---+")
  forM_ [0 .. height-1] $
    \y -> do
      putStr "|"
      forM_ [0 .. width-1] $
        \x -> do
          if fst start == x && snd start == y
            then putStr " s "
            else if fst goal == x && snd goal == y 
              then putStr " g "
              else putStr "   "
          hasRightWall <- readArray rightWalls (x, y)
          putStr (if hasRightWall then "|" else " ")
      putStrLn ""
      forM_ [0 .. width-1] $
        \x -> do
          putStr "+"
          hasBottomWall <- readArray bottomWalls (x, y)
          putStr (if hasBottomWall then "---" else "   ")
      putStrLn "+"




getStartPoint :: Int -> Int -> IO (Int, Int)
getStartPoint w h = do
    x <- randomRIO (0, w-1) :: IO Int
    y <- randomRIO (0, h-1) :: IO Int
    return (x, y)

getDirection :: (Int, Int) -> (Int, Int) -> Char
getDirection (x1, y1) (x2, y2)
    | x1 < x2 = 'R'
    | x1 > x2 = 'L'
    | y1 < y2 = 'U'
    | otherwise = 'D'


neighbours :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbours (x,y) width height = 
    (if x == 0 then [] else [(x-1, y)]) ++
    (if x == width-1 then [] else [(x+1, y)]) ++
    (if y == 0 then [] else [(x, y-1)]) ++
    (if y == height-1 then [] else [(x, y+1)])