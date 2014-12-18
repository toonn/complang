import Color
import Text

-- default values, units are pixels
text   = { blink = 50 } -- percentage
field  = { width = 1024,
           height = 768,
           bgcolor = black }
player = { w = 64,
           h = 16 }
tail   = { width = 2,
           length = 500 }

data Orientation = N | E | S | W

background = constant (filled field.bgcolor (rect field.width field.height))

hud = lift2 (\ b t -> if b then t else "")
            blink
            (constant (toForm (centered (Text.color white
                                            (toText "Not playing")))))

blink = foldp (\ c l -> xor c l)
              True
              (sampleOn (every (100*millisecond)) (constant True))

showGrid : Signal Element
showGrid = let forms = [background, hud]
            in collage (truncate field.width)
                       (truncate field.height)
                       forms

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

