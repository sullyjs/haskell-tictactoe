{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

 {-# LANGUAGE TupleSections #-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (MkRose x _) = x

children :: Rose a -> [Rose a]
children (MkRose _ x) = x

-- Exercise 2

size :: Rose a -> Int
size (MkRose _ x) = 1 + sum (map size x)

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ x) = sum (map leaves x)

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((a,b,c),(d,e,f),(g,h,i)) = ((a,d,g),(b,e,h),(c,f,i))

diagonals :: Board -> (Row, Row)
diagonals ((a,_,b),(_,c,_),(d,_,e)) = ((a,c,e),(b,c,d))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B,B,B),(B,B,B),(B,B,B))

-- Exercise 7

printRow :: Row -> String
printRow (a,b,c) = (show a) ++ "|" ++ (show b) ++ "|" ++ (show c) ++ "\n"

printHorSep :: String
printHorSep = "-+-+-\n"

printBoard :: Board -> String
printBoard (a,b,c) = printRow a ++ printHorSep ++ printRow b ++ printHorSep ++ printRow c

-- | Move generation

-- Exercise 8

tripleCycle :: (a,a,a) -> (a,a,a)
tripleCycle (a,b,c) = (b,c,a)

tripleDo :: ((a,a,a) -> [(a,a,a)]) -> (a,a,a) -> [(a,a,a)]
tripleDo func triple = (func triple) ++
                (map (tripleCycle . tripleCycle) $ func (tripleCycle triple)) ++
                (map tripleCycle  $ func (tripleCycle . tripleCycle $ triple))

movesField :: Field -> Row -> [Row]
movesField field (a,b,c) | a == B = [(field,b,c)]
                         | otherwise = []

movesRow :: Field -> Row -> [Row]
movesRow field row =  tripleDo (movesField field) row

movesBoard :: Field -> Board -> [Board]
movesBoard field (a,b,c) = map (,b,c) (movesRow field a)

moves :: Player -> Board -> [Board]
moves player board = tripleDo (movesBoard field) board
    where field = symbol player

-- | Gametree generation

-- Exercise 9

hasWinnerRow :: Row -> Maybe Player
hasWinnerRow (a,b,c) | a == b && b == c && c == symbol P1 = Just P1
                     | a == b && b == c && c == symbol P2 = Just P2
hasWinnerRow (_,_,_) = Nothing

hasWinnerRows :: Board -> Board -> (Row,Row) -> Maybe Player
hasWinnerRows (a,b,c) (d,e,f) (g,h) | hasWinnerRow a /= Nothing = hasWinnerRow a
                                    | hasWinnerRow b /= Nothing = hasWinnerRow b
                                    | hasWinnerRow c /= Nothing = hasWinnerRow c
                                    | hasWinnerRow d /= Nothing = hasWinnerRow d
                                    | hasWinnerRow e /= Nothing = hasWinnerRow e
                                    | hasWinnerRow f /= Nothing = hasWinnerRow f
                                    | hasWinnerRow g /= Nothing = hasWinnerRow g
                                    | hasWinnerRow h /= Nothing = hasWinnerRow h
                                    | otherwise = Nothing

hasWinner :: Board -> Maybe Player
hasWinner board = hasWinnerRows board (verticals board) (diagonals board)

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree player board | hasWinner board /= Nothing = MkRose board []
                      | otherwise = MkRose board (map (gameTree $ nextPlayer player) (moves player board))

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)

-- | Minimax

-- Exercise 12

minimax' :: Player -> Player -> Rose Board -> Rose Int
minimax' player _ (MkRose board []) | hasWinner board == Just player = MkRose 1 []
                                    | hasWinner board == Nothing = MkRose 0 []
                                    | otherwise = MkRose (-1) []
minimax' player currentPlayer (MkRose board boardList) = MkRose (mm (map root scoreList)) scoreList
    where scoreList = map (minimax' player (nextPlayer currentPlayer)) boardList
          mm = if player == currentPlayer then maximum' else minimum'

minimax :: Player -> Rose Board -> Rose Int
minimax player rose = minimax' player player rose

-- * Lazier minimum and maximums

-- Exercise 13

minimum'helper :: Int -> [Int] -> Int
minimum'helper x [] = x
minimum'helper _ (-1:_) = -1
minimum'helper current (x:rest) | x < current  = minimum'helper x rest
                                | otherwise = minimum'helper current rest

minimum' :: [Int] -> Int
minimum' list = minimum'helper 1 list

maximum'helper :: Int -> [Int] -> Int
maximum'helper x [] = x
maximum'helper _ (1:_) = 1
maximum'helper current (x:rest) | x > current = maximum'helper x rest
                                | otherwise = maximum'helper current rest

maximum' :: [Int] -> Int
maximum' list = maximum'helper (-1) list

-- | Gameplay

-- Exercise 14

findBoard :: Int -> [Rose Board] -> [Rose Int] -> Maybe Board
findBoard _ [] [] = Nothing
findBoard target ((MkRose board _):boards) ((MkRose score _):scores) | target == score = Just board
                                                                     | otherwise = findBoard target boards scores

makeMove :: Player -> Board -> Maybe Board
makeMove player board = findBoard (root scoreTree) (children boardTree) (children scoreTree)
    where boardTree = gameTree player board
          scoreTree = minimax player boardTree

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType 
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b) 
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y
