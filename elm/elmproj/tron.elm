import Color
import Text
import Keyboard
import Debug

-- default values, units are pixels
text   = { tRatio = 66, -- percentage
           freq = 0.5 }
field  = { width = 1024,
           height = 768,
           bgcolor = black }
player = { w = 64,
           h = 16 }
tail   = { width = 2,
           length = 500 }

data Orientation = N | E | S | W
data GamePhase = Pre | Mid | Post

-- This signal emits a bool freq/10 times per millisecond.
-- This bool is tRatio% of the time true, false otherwise.
blinkTF : Int -> Float -> Signal Bool
blinkTF tRatio freq = let period = (1000/freq)*millisecond
                       in merge (sampleOn (every period)
                                           (constant True))
                                 (delay ((toFloat tRatio)/100*period)
                                        (sampleOn (every period)
                                                  (constant False)))

-- This function converts a string into a blinking string.
blink : String -> Signal String
blink string = lift (\ bTF -> if bTF then string else "")
                    (blinkTF text.tRatio text.freq)


-- This signal emits Pre, Mid or Post according to which phase of the game
-- starts.
phase : Signal GamePhase
phase = let spacePress = keepIf identity False Keyboard.space
         in foldp (\ sp prev -> case prev of
                                Pre -> Mid
                                Mid -> Pre)
              Pre
              spacePress


-- The background of the "grid" a plain black rectangle of fixed size.
background : Signal [Form] -> Signal [Form]
background = lift ((::) (filled field.bgcolor <| rect field.width field.height))


-- Entry text signal.
entry : Signal Form
entry = (lift (toForm << centered << Text.color white << toText)
                       (blink "Space to Enter\nThe Grid"))

-- Score display signal.
score : Signal Form
score = (lift (toForm << centered << Text.color white << toText)
                       (blink "score"))

-- The hud displays the status (in text) of the game.
hud : Signal [Form] -> Signal [Form]
hud = lift2 (::) ((\ ph e c s -> case ph of
                                    Pre -> e
                                    Mid -> c
                                    Post -> s)
                    <~ phase
                     ~ entry
                     ~ (lift (toForm << centered << Text.color white << toText) (blink
                     "mid"))
                     ~ score)

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

