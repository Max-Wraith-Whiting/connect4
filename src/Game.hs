{- Game logic / win checking -}
module Game(module Game) where
import Data.List (isInfixOf)

{- Board and counters definition -}
{- In our case, we represent a board as a function from column IDs to lists of tokens.
 - These are sparse and grow upwards. -}
{- Row indices again begin at the bottom left and grow upwards -}
type RowID = Int
type ColumnID = Int
type RowCount = Int
type ColCount = Int
data Player = Red | Yellow
    deriving (Eq)

data Board = MkBoard { board :: [[Player]], numRows :: Int, numCols :: Int }

{- Toggles the current player -}
togglePlayer :: Player -> Player
togglePlayer Red = Yellow
togglePlayer Yellow = Red

{- Board accessors / manipulation function -}

-- Very useful mapping function that discards Nothing values.
justMap :: (a -> Maybe b) -> [a] -> [b]
justMap _ [] = []
justMap f (x:xs) = case f x of
    Nothing -> justMap f xs
    Just y  -> y:justMap f xs

emptyBoard :: RowCount -> ColCount -> Board
emptyBoard rs cs = (MkBoard { board = replicate cs [], numRows = rs, numCols = cs})

{- getCounter
 - Gets the counter at the given co-ordinates (or Nothing if there is no counter there).
 - Raises an error if co-ordinates are out-of-bounds. -}
getCounter :: Board -> RowID -> ColumnID -> Maybe Player
getCounter b r c
    | r > numRows b - 1|| r < 0 = error "Row ID out of bounds."
    | c > numCols b - 1|| c < 0 = error "Column ID out of bounds."
    | otherwise =
        if length (board b !! c) - 1 >= r
            then Just (board b !! c !! r)
        else
            Nothing

{- getRow
 - Retrieves the list of counters on the given row -}
getRow :: Board -> RowID -> [Maybe Player]
getRow b r
    | r >= numRows b || r < 0 = error "Row ID out of bounds."
    | otherwise = map (getCounter b r) [0..(numCols b - 1)]

{- getColumn
 - Retrieves the list of counters in the given column, from top-to-bottom -}
getColumn :: Board -> ColumnID -> [Maybe Player]
getColumn b c
    | c >= numCols b || c < 0 = error "Column ID out of bounds."
    | otherwise = map (\x -> getCounter b x c) [0..(numRows b - 1)]

{- Show instance for players -}
instance Show Player where
    show Red = "R"
    show Yellow = "Y"

{- Instance -}
instance Show Board where
    show b = concatMap (getRowString b) (reverse [0..(numRows b - 1)])

getRowString :: Board -> RowID -> String
getRowString b r = concatMap (maybe "0" show) (getRow b r) ++ "\n"

{- Drops a counter into the given column. If the move is legal, returns an updated
 - board. Otherwise returns Nothing. -}
dropCounter :: Board -> ColumnID -> Player -> Maybe Board
dropCounter b c p
    | c > (numCols b - 1) || c < 0 = Nothing
    | length (justMap id (getColumn b c)) >= numRows b = Nothing
    | otherwise =
        let (front, back) = splitAt c (board b) in
        let newBoard = front ++ [head back ++ [p]] ++ tail back in
            Just (b {board = newBoard})

getB :: Maybe Board -> Int -> Int -> Board
getB b r c = case b of
    Just x -> x
    Nothing -> emptyBoard r c

testDropCounter :: Int -> Int -> Board
testDropCounter r c =
    let empty = emptyBoard r c in
    let b1 = getB (dropCounter empty 0 Red) r c in
        b1

{- Diagonals -}
getTLBRDiagonals :: Board -> [[Maybe Player]]
getTLBRDiagonals b =
    let r_upper = numRows b - 1 in
    let c_upper = numCols b - 1 in
    let r_positions = [0..r_upper] ++ replicate c_upper r_upper in
    let c_positions = replicate r_upper 0 ++ [0..c_upper] in
    zipWith3 (getTLBR b) r_positions c_positions (repeat [])

getTLBR :: Board -> RowID -> ColumnID -> [Maybe Player] -> [Maybe Player]
getTLBR b r c diag =
    let c_upper = numRows b - 1 in
    let r_upper = numCols b - 1 in
    if c > c_upper || c < 0 || r > r_upper || r < 0 then
        diag
    else
        getTLBR b (r - 1) (c + 1) diag ++ [getCounter b r c]

getBLTRDiagonals :: Board -> [[Maybe Player]]
getBLTRDiagonals b =
    let r_upper = numRows b - 1 in
    let c_upper = numCols b - 1 in
    let r_positions = reverse [0..r_upper] ++ replicate c_upper 0 in
    let c_positions = replicate r_upper 0 ++ [0..c_upper] in
    zipWith3 (getBLTR b) r_positions c_positions (repeat [])

getBLTR :: Board -> RowID -> ColumnID -> [Maybe Player] -> [Maybe Player]
getBLTR b r c diag =
    let c_upper = numCols b - 1 in
    let r_upper = numRows b - 1 in
    if c > c_upper || r > r_upper then
        diag
    else
        getBLTR b (r + 1) (c + 1) diag ++ [getCounter b r c]

{- Win checking -}
{- Checks if the given list has a subsequence of length 4, returning Just Player
 - if so, Nothing otherwise -}
hasFourInRow :: [Maybe Player] -> Maybe Player
hasFourInRow l
  | [True, True, True, True] `isInfixOf` map getRedPlayer l = Just Red
  | [True, True, True, True] `isInfixOf` map getYellowPlayer l = Just Yellow
  | otherwise = Nothing

getRedPlayer :: Maybe Player -> Bool
getRedPlayer p = case p of
    Just Red -> True
    Just Yellow -> False
    Nothing -> False

getYellowPlayer :: Maybe Player -> Bool
getYellowPlayer p = case p of
    Just Yellow -> True
    Just Red -> False
    Nothing -> False

{- Checks all rows, columns, and diagonals for any subsequences of length 4 -}
checkWin :: Board -> Maybe Player
checkWin b =
    let rows = checkWinRows b in
    let cols = checkWinCols b in
    let bltr = checkWinBLTRDiagonals b in
    let tlbr = checkWinTLBRDiagonals b in
    let l = [rows, cols, tlbr, bltr] in

    if null (justMap id l) then
        Nothing
    else
        Just (head (justMap id l))


checkWinRows :: Board -> Maybe Player
checkWinRows b =
    let x = justMap (hasFourInRow . getRow b) [0..(numRows b - 1)] in
    if null x then Nothing
    else Just (head x)

checkWinCols :: Board -> Maybe Player
checkWinCols b =
    let x = justMap (hasFourInRow . getColumn b) [0..(numCols b - 1)] in
    if null x then Nothing
    else Just (head x)

checkWinTLBRDiagonals :: Board -> Maybe Player
checkWinTLBRDiagonals b =
    let x = justMap hasFourInRow (getTLBRDiagonals b) in
    if null x then Nothing
    else Just (head x)

checkWinBLTRDiagonals :: Board -> Maybe Player
checkWinBLTRDiagonals b =
    let x = justMap hasFourInRow (getBLTRDiagonals b) in
    if null x then Nothing
    else Just (head x)
