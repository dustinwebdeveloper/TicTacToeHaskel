import Data.List

{-

gameBoard = [[1,2,3],
             [4,5,6],
	     [7,8,9]]
1 | 2 | 3
---------
4 | 5 | 6
---------
7 | 8 | 9
-}

-- An open piece has an integer (1+index), Player piece has X or O

data Piece
    = Open Int
    | Player Char
    deriving Eq

-- Define show for Pieces, so it displays constructor arguments
instance Show Piece where
    show (Open n) = show n
    show (Player c) = [c]

-- Removes the Nth item (index being N-1) from a list
removeNth :: Int -> [a] -> ([a], [a])
removeNth index lst = (left, right)
    where
        (left, ys) = splitAt (index - 1) lst
        right = drop 1 ys

-- Given a board, piece, and index to place it in, place piece
-- at the position N (index bring N - 1)
placePiece :: [a] -> a -> Int -> [a]
placePiece board piece n = xs ++ [piece] ++ ys
    where (xs, ys) = removeNth n board 

-- Returns true if piece given is an Open piece
pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _)  = True
pieceIsOpen _         = False

-- Return true if the index in this board is open (index is N - 1)
openSpace :: [Piece] -> Int -> Bool
openSpace board index
    | length board < i         = False
    | pieceIsOpen $ board !! i = True
    | otherwise                = False
    where i = index - 1

-- Given a game board, get a valid position to place a piece 
-- there's no board type yet, board is a list of Pieces
getPiecePosition :: [Piece] -> IO Int
getPiecePosition board = do
    input <- getChar
    -- If input is a single digit, return as Int, otherwise get input again
    if input `elem` ['1' .. '9'] && openSpace board (read [input])
        then return (read [input])
        else do
            putStrLn "Enter an open position (1-9):"
	    getPiecePosition board

-- Makes a single line of three items in a board list
showBoardLine :: [Piece] -> String
showBoardLine (a:b:c:xs) = (show a) ++ " | " ++ (show b) ++ " | " ++ (show c)
showBoardLine _ = error "List must contain at least three elements"

-- Boarder to seperate board lines
boardBorder :: String
boardBorder = "\n---------\n"

-- Given the board, turn that board into a string to print out
showBoard :: [Piece] -> String
showBoard board = concat $ intersperse boardBorder $ [top, middle, bottom]
    where 
        top = showBoardLine board
        middle = showBoardLine (drop 3 board)
        bottom = showBoardLine (drop 6 board)

-- Given current character char (piece), give other character char (piece)

swapPlayers :: Char -> Char
swapPlayers 'X' = 'O'
swapPlayers 'O' = 'X'
swapPlayers _ = error "swapPlayers only accepts the characters O or X"

-- Given a board, player piece, and position on board, check if
-- the player given won vertically starting from the given position
checkWonVertically :: [Piece] -> Piece -> Int -> Bool
checkWonVertically board player index = topPos == player && middlePos == player && bottomPos == player
    where
	topPos = (board !! index)
	middlePos = board !! (index + 3)
	bottomPos = board !! (index + 6)

-- will return true if the player given won at all vertically
playerWonVertically :: [Piece] -> Piece -> Bool
playerWonVertically board player = or $ map (checkWonVertically board player) [0,1,2]

-- Given a board, player, and position, check if the
-- player won by making a full row of their piece
checkWonHorizontally :: [Piece] -> Piece -> Int -> Bool
checkWonHorizontally board player index = firstPos == player && secondPos == player && thirdPos == player
    where
	firstPos = board !! index
	secondPos = board !! (index + 1)
	thirdPos = board !! (index + 2)

-- will return true if the player given won at all horizontally
playerWonHorizontally :: [Piece] -> Piece -> Bool
playerWonHorizontally board player = or $ map (checkWonHorizontally board player) [0, 3, 6]

{- [a, b, c,            [i, h, g,
    d, e, f, (reversed)  f, e, d,
    g, h, i]             c, b, a
-}

-- Given board, player, starting pos, and step, teturn true if the next three
-- pieces on the board are all the player's piece (they won)
checkWonDiagonally :: [Piece] -> Piece -> Int -> Int -> Bool
checkWonDiagonally board player index step = firstPos == player && secondPos == player && thirdPos == player
    where
	firstPos = board !! index
	secondPos = board !! (index + step)
	thirdPos = board !! (index + 2 * step)

-- Given board, player, return true if they won at all diagonally
playerWonDiagonally :: [Piece] -> Piece -> Bool
playerWonDiagonally board player = wonFirstDiagonal || wonSecondDiagonal
    where
        wonFirstDiagonal = checkWonDiagonally board player 0 4
	wonSecondDiagonal = checkWonDiagonally board player 2 2

-- Given a board and a player, return true if they won at all
playerWon :: [Piece] -> Piece -> Bool
playerWon board player = playerWonDiagonally board player || playerWonHorizontally board player || playerWonVertically board player

-- Return true if the game has become a tie
tieGame :: [Piece] -> Bool
tieGame board = all (\piece -> not (pieceIsOpen piece)) board

-- Check if anyone won/tied, if not, continues game
checkBoardState :: [Piece] -> Char -> IO ()
checkBoardState board playerChr
    | tieGame board                = putStrLn "It's a tie!"
    | playerWon board (Player 'x') = putStrLn "Player X won!"
    | playerWon board (Player 'O') = putStrLn "Player O won!"
    | otherwise                    = runTicTacToe board (swapPlayers playerChr)

-- Main loop that actually runs the tic tac toe game
runTicTacToe :: [Piece] -> Char -> IO ()
runTicTacToe board playerChr = do
    putStrLn $ showBoard board
    rawChoice <- getPiecePosition board
    -- Create the new board after placing the player's piece on it	
    let newBoard = placePiece board (Player playerChr) rawChoice
    -- Check if anyone won, if not, loop again
    checkBoardState newBoard playerChr

{-
    putStrLn $ showBoard board
    rawChoice <- getPiecePosition
    let newBoard = placePiece board (Player chr)
    checkBoardState newBoard
-}

{-
[[Pieces]]
[[1,2,3],[4,5,6],[7,8,9]]
Lst = [1,2,3,4,5,6,7,8,9]
splitAtNth Lst 5
([1,2,3,4], [6,7,8,9])
joinWithPiece (xs,ys) piece = xs ++ [piece] ++ ys
-}

main :: IO ()
main = runTicTacToe board 'X'
    where board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]














