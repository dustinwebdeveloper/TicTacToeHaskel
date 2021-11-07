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

runTicTacToe :: [Piece] -> Char -> IO ()
runTicTacToe = undefined {-
    >runTicTacToe will be recursively called
    rawChoice <- getPiecePosition
    let newBoard = placePiece board (Player chr) rawChoice
    checkBoardState newBoard chr
-}

main :: IO ()
main = undefined