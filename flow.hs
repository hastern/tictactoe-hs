-- --------------------------------------------------
-- Tic Tac Toe
--
-- A simple tic tac toe, written in haskell
--
-- --------------------------------------------------

module Flow (Game, GameFunc, play, humanPlayer, place, doMove) where

import Datatypes
import Logic
import Print (printBoard)

import Data.Maybe
import Data.List
import Control.Monad
import Control.Exception 


-- Simple play function: start game and print outcome.

play		:: GameFunc -> GameFunc -> IO ()
play f1 f2	= do
  putStrLn "\nTic Tac Toe\n============\n\nFirst player with 3 in a row wins the game\n"
  putStr "\nValid positions: "
  print [UL, UM, UR, ML, MM, MR, LL, LM, LR]
  putStrLn "\n\n"
  g <- gameRound (Live emptyBoard) X f1 f2
  case g of 
    (Won m b) 	-> putStrLn ((show m) ++ " has won the game!")
    (Draw b) 	-> putStrLn "The game is a draw!"
  printBoard $ getBoard g

-- A game is played on a board by a player with a marking.
-- The result is a game "Draw" or a "Won".
-- Since their can be two different parties in the game this take two
-- game functions, one for each party.

gameRound 		:: Game Board -> Marking -> GameFunc -> GameFunc -> IO (Game Board)
gameRound g m f1 f2	= do
  g' <- f1 g m 
  if win m $ getBoard g' then do 
    return (Won m $ getBoard g') 
  else 
    if isDraw $ getBoard g' then 
      return (Draw $ getBoard g') 
    else 
      gameRound g' (nextPlayer m) f2 f1


humanPlayer 			:: GameFunc
humanPlayer g@(Won m b) p	= return g
humanPlayer g@(Draw b) p	= return g
humanPlayer g@(Live b) m	= do
  printBoard b
  putStr ("\nPlayer " ++ (show m) ++ ", ")
  pos <- readPosition
  doMove m pos b humanPlayer

-- A move is done by placing a marking on a position on the board.
-- In the case of an error during the placement, the action can redone by calling
-- the specific game function.

doMove		:: Marking -> Position -> Board -> GameFunc -> IO (Game Board)
doMove m p b f	=
  case place p m b of
    Nothing  -> do
      putStrLn "Invalid move"
      f (Live b) m
    Just game -> return game

-- Reading a position is done in two steps:
-- 1. read the input line into a string
-- 2. parse the string into a position

readPosition	:: IO Position
readPosition	= do
  putStr "enter position:"
  raw <- try getLine :: IO (Either IOException String)
  putStr "\n\n"
  case raw of 
    Left _ -> do  -- Exception
      putStrLn ("Error:")
      readPosition
    Right rawStr -> do -- read string
      pos <- try $ evaluate $ read rawStr :: IO (Either ErrorCall Position)
      case pos of
        Left _ -> do
          putStrLn "Invalid position! "
          readPosition
        Right p -> do
          return p

-- Place a marking on a position on the board. If the move is valid a game is return

place		:: Position -> Marking -> Board -> Maybe (Game Board)
place p m []	= Just $ return [(p,m)]
place p m b
  | validMove p b	= Just $ return ((p,m):b)
  | otherwise		= Nothing

