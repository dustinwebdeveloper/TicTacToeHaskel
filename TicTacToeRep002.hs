import Data.List

{-
1 | 2 | 3
---------
4 | 5 | 6
---------
7 | 8 | 9
-}

{-
gameBoard =
    [[1, 2, 3],
     [4, 5, 6],
     [7, 8, 9]]

Open 1 | Player O | Open 3
-}

-- An open piece has an integer (1+index), Player piece, which is X or O

{- [[Pieces]]
   [[1,2,3],[4,5,6],[7,8,9]]

   lst = [1, 2, 3, 4, 5, 6, 7, 8, 9]
   splitAtNth Lst 5
   ([1, 2, 3, 4], [6, 7, 8, 9])
   joinWithPiece (xs, ys) piece = xs ++ [piece] ++ ys
-}

data Piece
    = Open Int
    | Player Char
    deriving Eq
    
-- [Piece 1, Piece 2, .. Piece 9] 
-- Define show for pieces, so it displays constructor arguments
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
-- at the position N (index being N - 1)
-- board is a list of pieces (Piece = Player 'O' | Open 1)
placePiece :: [a] -> a -> Int -> [a]
placePiece board piece index = xs ++ [piece] ++ ys
    where (xs, ys) = removeNth index board

-- Returns true if piece given is an Open piece
pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _) = True
pieceIsOpen _        = False

-- Return true if the index in this board is open (index is N - 1)
openSpace :: [Piece] -> Int -> Bool
openSpace board index
    | length board < i               = False
    | pieceIsOpen $ board !! i       = True
    | otherwise                      = False
    where i = index - 1

-- Given a game board, get a valid position to place a piece
getPiecePosition :: [Piece] -> IO Int
getPiecePosition board = do
    putStrLn "Enter an open position (1-9):"
    input <- getChar
    -- If input is a single digit, return as int, otherwise get input again
    if input `elem` ['1' .. '9'] && openSpace board (read [input])
        then return (read [input])
	else getPiecePosition board	    

-- Makes a single line of three items in a board list
showBoardLine :: [Piece] -> String
showBoardLine (a:b:c:xs) = (show a) ++ " | " ++ (show b) ++ " | " ++ (show c)
showBoardLine _ = error "List must contain at least three elements"

-- Border to seperate board lines
boardBorder :: String
boardBorder = "\n---------\n"

-- Given the board, turns that board into a string to print out
showBoard :: [Piece] -> String
showBoard board = concat $ intersperse boardBorder $ [top, middle, bottom]
    where
        top = showBoardLine board
	middle = showBoardLine (drop 3 board)
	bottom = showBoardLine (drop 6 board)

runTicTacToe :: [Piece] -> Char -> IO ()
runTicTacToe board playerChr = undefined {-
    >runTicTacToe will be recursively called
    putStrLn $ showBoard board
    rawChoice <- getPiecePosition board
    let newBoard = placePiece board (Player chr) rawChoice
    checkBoardState newBoard chr
-}

main :: IO ()
main = undefined