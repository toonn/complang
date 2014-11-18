-- Modify main below, so that it draws a little person
-- figure. Use the functions from the Collage module to construct
-- Forms (circles, n-gons, lines etc.) to include in the collage.
-- You can find documentation here:
--    http://library.elm-lang.org/catalog/elm-lang-Elm/0.13/Graphics-Collage

main : Signal Element
main = constant (collage 200 200
                    [group [
                    outlined (solid brown) (oval 15 30),
                    traced (solid red) (path [(0,-15),(0,-60)]),
                    traced (solid blue) (path [(0,-20),(-30, -50)]),
                    traced (solid green) (path [(0,-20),(30, -50)]),
                    traced (solid orange) (path [(0,-60),(-20,-100)]),
                    traced (solid purple) (path [(0,-60),(20,-100)])]])
