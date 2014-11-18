-- Make a program that draws a 500x500 black rectangle with a small
-- white 10x10 rectangle in the middle, so that you can move the small
-- rectangle with the keyboard arrows. Make use of the functions
-- Keyboard.arrows, Collage.collage, Collage.rect, Collage.filled,
-- Collage.move, Signal.foldp, toFloat and Signal.lift.
import Keyboard

showState : (Int, Int) -> Element
showState (x, y) = collage 500 500 [
                        filled black (rect 500 500),
                        move (toFloat x, toFloat y) (filled white (rect 10 10))
                        ]


state : Signal (Int, Int)
state = foldp (\arrows (x,y) ->
                        (if (x + arrows.x > -240) && (x + arrows.x < 240) then x + arrows.x else x,
                            if (y + arrows.y > -240) && (y + arrows.y < 240) then y + arrows.y else y))
                (0,0)
                (sampleOn (fps 60) Keyboard.arrows)


main : Signal Element
main = lift showState state
