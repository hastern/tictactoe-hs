-- --------------------------------------------------
-- Tic Tac Toe
--
-- A simple tic tac toe, written in haskell
--
-- --------------------------------------------------

module Print (printBoard, printGameTree) where

import Datatypes


import Data.Maybe
import Data.List

-- Printing the border.

printBorder		:: IO ()
printBorder		= putStr "\n---+---+---\n"

-- Print a cell and the marking inside (if their is one)

printCell		:: Maybe Marking -> IO ()
printCell Nothing	= putStr "   "
printCell (Just m)	= if (m == X) then putStr " X " else putStr " O "

-- Print the space between the cells.

printSpace	:: Position -> IO ()
printSpace p	= if p == UR || p == MR  then printBorder else putChar '|'

-- Print the board.

printBoard	:: Board -> IO ()
printBoard b	= printBoard' (sort b) UL
  where 
    printBoard' 		:: Board -> Position -> IO ()
    printBoard' b LR		= do 
      if null b then printCell Nothing
        else printCell (Just (snd $ head b))
      putChar '\n'
    printBoard' [] i		= do
      printCell Nothing
      printSpace i
      printBoard' [] (succ i)
    printBoard' b@(p:ps) i	= 
      if (fst p) == i then do 
        printCell (Just (snd p))
        printSpace i
        printBoard' ps (succ i)
      else do 
        printCell Nothing
        printSpace i
        printBoard' b (succ i)

-- Print a game status

printGame		:: Game Board -> IO ()
printGame (Live _)	= putStr "Live"
printGame (Won m _)	= putStr ("Win " ++ (show m))
printGame (Draw _)	= putStr "Draw"

-- Print a game tree

printGameTree	:: GameTree -> Int -> IO ()
printGameTree	= printGameTree' "" 
    where 
      printGameTree' :: String -> GameTree -> Int -> IO ()
      printGameTree' s (Move g ts) n	= do
        putStr ("\n" ++ s ++ "+" ++ (show $ getBoard g) ++ "->" )
        printGame g
        if n > 0 then printSubTrees (s ++ "|  ") ts (n-1) else putStr ""
      printSubTrees		:: String -> [GameTree] -> Int -> IO ()
      printSubTrees s [] n	= putStr ""
      printSubTrees s (t:ts) n	= do
        printGameTree' s t n 
        printSubTrees s ts n 

