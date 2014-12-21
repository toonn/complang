import Color
import Text

-- default values, units are pixels
text   = { tRatio = 50, -- percentage
           freq = 0.2 }
field  = { width = 1024,
           height = 768,
           bgcolor = black }
player = { w = 64,
           h = 16 }
tail   = { width = 2,
           length = 500 }

data Orientation = N | E | S | W

-- This signal emits a bool freq times per second.
-- This bool is tRatio% of the time true, false otherwise.
blinkTF : Float -> Float -> Signal Bool
blinkTF tRatio freq = foldp (xor)
                            True
                            (sampleOn (every (1000/freq*millisecond))
                                      (constant True))

-- This function converts a string into a blinking string.
blink : String -> Signal String
blink string = keepWhen (blinkTF text.tRatio text.freq)
                      ""
                      (constant string)


-- The background of the "grid" a plain black rectangle of fixed size.
background : Signal [Form] -> Signal [Form]
background = lift ((::) (filled field.bgcolor <| rect field.width field.height))

-- The hud displays the status (in text) of the game.
hud : Signal [Form] -> Signal [Form]
hud = lift2 (::) (lift (toForm << centered << Text.color white << toText)
                       (blink "Not Playing"))


-- The grid is the combination of all elements in the game that should be
-- displayed.
grid : Signal [Form]
grid = background << hud <| constant []

-- This signal turns the forms in the grid into an html element that can be
-- displayed.
showGrid : Signal Element
showGrid = lift (collage (truncate field.width) (truncate field.height))
                grid

-- example use: showPlayer' Color.lightBlue (10, 10) N
showPlayer' : Color -> (Float, Float) -> Orientation -> Form
showPlayer' color (x, y) o =
    let fw = toFloat player.w
        (xOffset, yOffset, degs) = case o of
                    N -> (0, fw / 2, 90)
                    E -> (fw / 2, 0, 0)
                    S -> (0, -fw / 2, -90)
                    W -> (-fw / 2, 0, 180)
    in rect (fw) (toFloat player.h) 
        |> outlined (solid color)
        |> move (x, y)
        |> move (xOffset, yOffset)
        |> rotate (degrees degs) 


-- example use: showTail Color.lightBlue [(10, 10), (20, 10)]
showTail : Color -> [(Float, Float)] -> Form
showTail color positions = 
    traced { defaultLine |
        width <- tail.width
        , color <- color
        } (path positions)

main = showGrid

