-- Step 1: Change the below code so that it counts the number of user clicks.
-- Step 2: Change the code so that it counts the number of user clicks
--         and space bar presses.
import Mouse
import Keyboard

clicks : Signal ()
clicks = Mouse.clicks

clickCount : Signal Int
clickCount = foldp (\_ acc -> acc + 1) 0 clicks

spaceCount : Signal Int
-- spaceCount = countIf (identity) Keyboard.space
spaceCount = foldp (\isdown acc -> if isdown then acc + 1 else acc)
                    0
                    Keyboard.space

main : Signal Element
main = lift2 above (lift asText clickCount) (lift asText spaceCount)
