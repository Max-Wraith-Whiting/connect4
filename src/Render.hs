module Render(module Render) where
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe (fromMaybe)

{- Viewport dimensions -}
windowWidth :: Float
windowWidth = 1024.0

windowHeight :: Float
windowHeight = 768.0

-- (Rendering the board)
render :: Board -> Picture
render b = _renderBoard b <> _renderCounters b

_renderBoard :: Board -> Picture
_renderBoard b =
    let r = numRows b in
    let c = numCols b in
    let r_f = fromIntegral r :: Float in
    let c_f = fromIntegral c :: Float in
    let cell_size = 80 in
    let cell = rectangleWire cell_size cell_size in
    let column =  foldr (<>) blank (zipWith (translate 0) [0.0, cell_size..(r_f * cell_size)] (replicate r cell)) in
    let full_board = foldr (<>) blank (zipWith3 translate [0.0, cell_size..(c_f * cell_size)] (repeat 0) (replicate c column)) in
        color blue (translate (-(cell_size * (c_f - 1) * 0.5)) (-(cell_size * (r_f - 1) * 0.5)) full_board)


_renderCounters :: Board -> Picture
_renderCounters b =
    let c_upper = numCols b - 1 in
    let r_upper = numRows b - 1 in
    let r_f = fromIntegral r_upper :: Float in
    let c_f = fromIntegral c_upper :: Float in
    let columns = [0..c_upper] in
    let merge = foldr (<>) blank in
    let center = translate (-(80 * c_f * 0.5)) (-(80 * r_f * 0.5)) in
    center (merge (zipWith3 translate [0,80..(c_f * 80)] (repeat 0) (map (_renderCounterColumn b) columns)))

_renderCounterColumn :: Board -> ColumnID -> Picture
_renderCounterColumn b c =
    let r_upper = numRows b - 1 in
    let r_f = fromIntegral r_upper :: Float in
    let merge = foldr (<>) blank in
    let column i = zipWith3 translate (repeat 0) [0.0, 80..(r_f * 80)] (map _renderCounter (getColumn b i)) in
    merge (column c)

_renderCounter :: Maybe Player -> Picture
_renderCounter p =
    case p of
        Just Red -> color red (circleSolid 35)
        Just Yellow -> color yellow (circleSolid 35)
        Nothing -> blank

-- (Game loop)
testPicture :: IO ()
testPicture =
    displayIO
        (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
        white
        (return (color red (rectangleSolid 50 50)))
        (const $ return ())

handleInput :: Event -> World -> IO World
handleInput (EventKey (Char x) Down _ _ ) w =
    let b = current_b w in
    let p = current_p w in
    let new_b index = fromMaybe b (dropCounter b index p) in
    let isWinner index = checkWin (new_b index) in
    let update_b index = return (World {isWon = isWinner index, current_b = new_b index, current_p = togglePlayer p}) in
    let restart = return (World {isWon = Nothing, current_b = emptyBoard (numRows b) (numCols b), current_p = Red}) in
    case x of
        '0' -> update_b 0
        '1' -> update_b 1
        '2' -> update_b 2
        '3' -> update_b 3
        '4' -> update_b 4
        '5' -> update_b 5
        '6' -> update_b 6
        '7' -> update_b 7
        '8' -> update_b 8
        '9' -> update_b 9
        'r' -> restart
        _ -> return w
handleInput _ w = return w

data World = World {
    isWon :: Maybe Player,
    current_p :: Player,
    current_b :: Board
}

renderWorld :: World -> Picture
renderWorld w =
    let win = isWon w in
    case win of
        Just Red -> translate (-320) 0 (text "Red Wins!")
        Just Yellow -> translate (-350) 0 (text "Yellow Wins!")
        Nothing -> render (current_b w)


gameLoop :: Int -> Int -> IO ()
gameLoop rows cols
    | rows > 10 = error "GUI cannot handle games with > 10 rows."
    | cols > 10 = error "GUI cannot handle games with > 10 columns."
    | otherwise =
        interactIO
        (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
        white
        (World {isWon = Nothing, current_b = emptyBoard rows cols, current_p = Red})
        (return . renderWorld)
        handleInput
        (const $ return ())