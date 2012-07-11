-- --------------------------------------------------
-- Tic Tac Toe
--
-- A simple tic tac toe, written in haskell
--
-- tic tac toe logic
--
-- --------------------------------------------------

module Logic(Position, Marking, Placement, Board, win, isDraw, validMove, nextPlayer) where

import Datatypes

import Data.List

--There are 8 different winning position, three vertical columns, three horizontal 
--rows and the two diagonals.

winPosition :: [[Position]]
winPosition = [ [UL,UM,UR], [ML,MM,MR], [LL,LM,LR], 
                 [UL,ML,LL], [UM,MM,LM], [UR,MR,LR],
                 [UL,MM,LR], [UR,MM,LL]
               ]


--To test if a given list of positions fits a winning position each subsequence 
--with three elements is tested againt all 8 winning patterns.
--The input list of positions gets sorted in the same way the winning patterns are
--sorted. Due to this, the order of placements is irrelevant.


testWin	:: [Position] -> Bool
testWin l	= testWin' $ filter (\x -> length x == 3) (subsequences (sort l))
  where 
    testWin'		:: [[Position]] -> Bool
    testWin' []	= False
    testWin' (ps:pss)	= if ps `elem` winPosition then True 
			else testWin' pss

--A player has won, if the positions of his markings fit a winning pattern.
--Therefore a list of "his" positions is generated out of the board.

win		:: Marking -> Board -> Bool
win m b	= win' m b []
  where 
    win'		:: Marking -> Board -> [Position] -> Bool
    win' m [] l	= testWin l
    win' m (p:ps) l	= if snd p == m	then win' m ps ((fst p):l) 
			else win' m ps l

--A game is a draw, if all 9 positions on the board are filled.

isDraw		:: Board -> Bool
isDraw b	= length b == 9

--A move is valid, if the position isn't already blocked by a another marking.

validMove		:: Position -> Board -> Bool
validMove p []		= True
validMove p (b:bs)	= if  p == fst b then False else validMove p bs

--A board is valid if it has no double placements.

validBoard		:: Board -> Bool
validBoard []		= True
validBoard (b:bs)	= if validMove (fst b) bs then validBoard bs else False


-- Switch the markings -> toggeling the active player.

nextPlayer	:: Marking -> Marking
nextPlayer X	= O
nextPlayer O 	= X
