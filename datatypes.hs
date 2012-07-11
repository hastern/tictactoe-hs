-- --------------------------------------------------
-- Tic Tac Toe
--
-- A simple tic tac toe, written in haskell
--
-- --------------------------------------------------

module Datatypes where


--A game can be definied by a list of positions and markings.

data Position 	= UL | UM | UR | ML | MM | MR | LL | LM | LR 
  deriving (Enum, Eq, Ord, Show, Read)
data Marking 	= X | O 
  deriving (Eq, Ord, Show)
type Placement = (Position, Marking)
type Board 	= [Placement]

emptyBoard :: Board
emptyBoard = []	

-- There are three possible states of a game:
-- - Won by a player
-- - A draw, because all fields are blocked, but no winning pattern is matched
-- - Still in progress
-- This monadic form allows sequential program flows.

data Game a = Won Marking a | Live a | Draw a deriving (Show)

instance Monad Game where 
  return		= Live
  (Won _ b)	>>= g	= g b
  (Draw b)	>>= g 	= g b
  (Live b)	>>= g	= g b
  
-- Accessor function for the board of a given game.

getBoard 		:: Game Board -> Board
getBoard (Live b)	= b
getBoard (Won _ b)	= b
getBoard (Draw b)	= b


-- A gaming function takes a game, and a Marking and creates a new game (-round)

type GameFunc = Game Board -> Marking -> IO (Game Board)

-- Like a human player an AI player must anticipate the next movements of its 
-- opponent.
-- 
-- A widely use technic to archive this, is a "Game tree".
-- Point of origin is a configuration of a board. Based on this configuration
-- a tree gets spanned, displaying all possible movements.
-- 
-- By traversing the tree, the AI is looking for a path of movements that lead to
-- a winning situation.

data GameTree = Move (Game Board) [GameTree] deriving (Show)

getSubTrees			:: GameTree -> [GameTree]
getSubTrees (Move _ ts)	= ts

getGame 		:: GameTree -> Game Board
getGame (Move g _)	= g

-- The ai uses the tree to follow some basic roles for playing.
-- 
-- 1. If the AI can win in 1 step, it will do so.
-- 2. If the human can win in his next step, the AI will prevent this.
-- 3. Pick the first valid position

data Chose a = Win a | Lose a | Random a | NoPos