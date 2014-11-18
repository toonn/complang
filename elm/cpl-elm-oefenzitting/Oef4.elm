-- The program below has a bug, it will be obvious when you run it.
-- The assignment is to use Elm's watch-expressions and the time-traveling
-- debugger to find and fix the bug.

-- As an example we placed a (useless) watch expression on the heartbeat value,
-- this allows you to observe its values in the debugger.

import Debug

heartbeat = lift (\x -> Debug.watch "Tijd: " x) (every (second/50))

update : (Int,Int) -> (Int,Int)
update (y,dy) = if y <= 0 then (y-dy,-dy)
                else if y >= 370 then (370-1, -100) else (y+dy,dy-1)

showState : (Int,Int) -> Element
showState (y,_) = collage 400 400 [
                     filled black (rect 400 400)
                   , move (0, (toFloat y)/2) (Debug.trace "trace: " ((filled white (oval 15 15))))
                   ]

state : Signal (Int,Int)
state = lift (\x -> Debug.watch "State: " x)
            (foldp (\_ s -> update s) (21,20) heartbeat)

main : Signal Element
main = lift showState state
