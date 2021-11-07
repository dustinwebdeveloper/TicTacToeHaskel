{-
gameBoard =
    [[1, 2, 3],
     [4, 5, 6],
     [7, 8, 9]]

Open 1 | Player O | Open 3
-}

-- An open piece has an integer (1+index), Player piece, which is X or O

data Piece
    = Open Int
    | Player Char
    deriving Eq
    
-- [Piece 1, Piece 2, .. Piece 9] 
-- Define show for pieces, so it displays constructor arguments
instance Show Piece where
    show (Open n) = show n
    show (Player c) = [c]

main :: IO ()
main = undefined