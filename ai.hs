-- --------------------------------------------------
-- Tic Tac Toe
--
-- A simple tic tac toe, written in haskell
--
-- --------------------------------------------------

module AI where

import Datatypes
import Logic 
import Flow 
import Print 

import Data.List

mkGameTree 		:: Marking -> Board -> GameTree
mkGameTree m b		= if win X b then Move (Won X b) [] else
			if win O b then Move (Won O b) [] else
			if isDraw b then Move (Draw b) [] else
			Move (Live b) $ subGameTrees m b

subGameTrees		:: Marking -> Board -> [GameTree]
subGameTrees m b	= subGameTrees' m b [UL, UM, UR, ML, MM, MR, LL, LM, LR] []
  where
    subGameTrees' 			:: Marking -> Board -> [Position] -> [GameTree] -> [GameTree]
    subGameTrees' m b [] ts		= ts
    subGameTrees' m b (p:ps) ts	= 
	case place p m b of
	Just (Live b')	-> subGameTrees' m b ps ((mkGameTree (nextPlayer m) b'):ts)
	Nothing		-> subGameTrees' m b ps ts


-- The AI implementation 

nextPosition 		:: Marking -> Board -> Chose Position
nextPosition m b 	= 
  case findWinPosition m t of
    (Just p)	-> Win p
    Nothing	-> 
      case findWinPosition m' t' of 
        (Just p) 	-> Lose p
        Nothing 	-> 
          case searchSubTrees m t of
            (Just p)	-> Win p
            Nothing 	-> nextValidPosition b
  where
    m' = (nextPlayer m)
    t = (mkGameTree m b)
    t' = (mkGameTree m' b)

-- Find a winning position for a marking in a game tree.
-- Returns maybe a position, if their is one.

findWinPosition				:: Marking -> GameTree -> Maybe Position
findWinPosition m (Move g [])			= Nothing
findWinPosition m (Move g@(Live b) (t:ts))	= 
  if win m $ getBoard $ getGame t then 
    Just (fst $ head ( (getBoard $ getGame t) \\ b ) )
  else 
    findWinPosition m (Move g ts)

-- Find the next valid position in the board.

nextValidPosition	:: Board -> Chose Position
nextValidPosition b	= nextValidPosition' b [MM,UL,UM,UL,MR,ML,LL,LM,LR]
  where 
    nextValidPosition'			:: Board -> [Position] -> Chose Position
    nextValidPosition' b []		= NoPos
    nextValidPosition' b (p:ps)	=
      if (validMove p b) then Random p else nextValidPosition' b ps

-- Search the subtrees for a winning position.

searchSubTrees			:: Marking -> GameTree -> Maybe Position
searchSubTrees	m (Move _ [])	= Nothing
searchSubTrees	m (Move g ts)	= searchSubTrees' m g ts
  where 
    searchSubTrees'		:: Marking -> Game Board -> [GameTree] -> Maybe Position
    searchSubTrees' m _ []	= Nothing
    searchSubTrees' m g (t:ts)	= 
      case findWinPosition m t of
        (Just p)	-> Just (fst $ head ( (getBoard $ getGame t) \\ getBoard g ) )
        Nothing 	-> searchSubTrees' m g ts

-- AI player.
-- Mostly information output

aiPlayer 		:: GameFunc
aiPlayer g@(Won _ _) p	= return g
aiPlayer g@(Draw _) p	= return g
aiPlayer g@(Live b) m 	= do
  printBoard b
  putStr "\n"
  case nextPosition m b of
    NoPos	-> do
      putStrLn ("AI(" ++ (show m) ++ ") surrenders (no chance of winning)")
      return (Won (nextPlayer m) b)
    Win p 	-> do
      putStrLn ("AI(" ++ (show m) ++ ") chose " ++ (show p) ++ " to win the game.")
      doMove m p b aiPlayer
    Lose p 	-> do
      putStrLn ("AI(" ++ (show m) ++ ") chose " ++ (show p) ++ " to prevent losing.")
      doMove m p b aiPlayer
    Random p 	-> do
      putStrLn ("AI(" ++ (show m) ++ ") chose " ++ (show p) ++ " at random.")
      doMove m p b aiPlayer

