-- Write a program that shows three text fields, arranged vertically.
-- The first shows the current mouse position and True or False
-- depending on whether the left mouse button is currently down. The
-- second text box is shown below the first and shows True or False
-- depending on whether or not the space bar is down. Finally, the
-- third text field shows the current value of Keyboard.arrows. Play
-- with the resulting program so that you understand the behaviour of
-- all these input signals.
import Mouse
import Keyboard

main = lift (flow down)
        (combine
        [lift2 beside (lift asText Mouse.position) (lift asText Mouse.isDown),
        lift asText Keyboard.space,
        lift asText Keyboard.arrows])
