import Color
import Text
import Keyboard
import Debug

-- default values, units are pixels
text       = { tRatio = 66, -- percentage
               freq = 0.5 }
field      = { width = 1024,
               height = 768,
               bgcolor = black }
lightcycle = { w = 16,
               h = 64,
               velocity = 150 } -- pixels per second
jetwall    = { width = 2,
               length = 500 }
fpers      = 30

data Orientation = N | E | S | W

data GamePhase = Pre | Mid | Post
type Place = (Float, Float) -- (x, y)
type Controls = (Keyboard.KeyCode, Keyboard.KeyCode) -- (left, right)
data Player = Player Color Place Orientation Controls
data Turn = Left | Right
type Velocity = Float

-- This signal emits a bool freq times per second.
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
blink string = (\ bTF -> if bTF then string else "")
                 <~ (blinkTF text.tRatio text.freq)


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
entry = (toForm << centered << Text.color white << toText)
          <~ (blink "Space to Enter\nThe Grid")

-- Score display signal.
score : Signal Form
score = (toForm << centered << Text.color white << toText)
          <~ (blink "__ has won!")

-- The hud displays the status (in text) of the game.
hud : Signal [Form] -> Signal [Form]
hud = lift2 (::) ((\ ph e c s -> case ph of
                    Pre -> e
                    Mid -> c
                    Post -> s)
                    <~ phase
                     ~ entry
                     ~ ((toForm << centered << Text.color white << toText)
                          <~ (blink "mid"))
                     ~ score)


-- Calculate a new orientation from an orientation and a turn.
turn : Turn -> Orientation -> Orientation
turn t o = case t of Left  ->
                       case o of
                         N -> W
                         W -> S
                         S -> E
                         E -> N
                     Right ->
                       case o of
                         N -> E
                         E -> S
                         S -> W
                         W -> N

-- Signal of turns corresponding to keypresses from the controls.
turns : Controls -> Signal Turn
turns (leftKey, rightKey) =
    let keyToTurns k t = sampleOn (keepIf (identity) False
                                     <| Keyboard.isDown k)
                                  (constant t)
     in merge (keyToTurns leftKey Left) (keyToTurns rightKey Right)

-- This signal represents the direction something is facing, which changes
-- with the controls.
directions : Orientation -> Signal Turn -> Signal Orientation
directions initialDirection = foldp (turn) initialDirection

-- Calculate a new place from a velocity (px/s), a place and an orientation.
mv : Velocity -> Orientation -> Place -> Place
mv v o (x, y) = let distance = v/(1000/25)
                 in case o of
                   N -> (x           , y + distance)
                   E -> (x + distance, y           )
                   S -> (x           , y - distance)
                   W -> (x - distance, y           )

-- This signal represents a position that changes with a certain velocity
-- in a certain direction.
places : Place -> Signal Orientation -> Signal Place
places startPlace directions =
    foldp (mv lightcycle.velocity)
          startPlace
          (sampleOn (every <| 25*millisecond) directions)

-- Signal of list representations of jetwalls (lightcycle tails).
trace : Float -> Place -> Signal Orientation -> Signal Place -> Signal [Place]
trace length initialPlace dirs plcs =
    let crop len xs =
            case foldl (\ (x, y) (ps, (px, py), l) ->
                         let distance = (abs (x-px)) + (abs (y-py))
                          in if l + distance <= len
                             then (ps ++ [(x,y)], (x,y), l + distance)
                             else let np =
                                    if | y == py && x < px -> (px - (len-l), py)
                                       | y == py && x > px -> (px + (len-l), py)
                                       |            y < py -> (px, py - (len-l))
                                       | otherwise         -> (px, py + (len-l))
                                   in (ps ++ [np], np, len))
                       ([], (head xs), 0)
                       xs of
              (ps, _, _) -> ps
     in fst <~ foldp (\ (dir, plc) (trc, lastdir) ->
                       (crop jetwall.length (if dir == lastdir
                                             then plc :: (drop 1 trc)
                                             else plc :: trc),
                       dir))
                     ([initialPlace], W)
                     --(delay (4/lightcycle.velocity*second)
                             -- approx. 4 px delay on tail
                     ((,) <~ dirs ~ plcs)--)

-- This signal represents a player.
player : Player -> Orientation -> Place -> [Place]
            -> [Form]
player (Player color _ _ _) dir plc trc =
     [(showPlayer' color plc dir), (showTail color trc)]

-- To resolve collisions players are processed in order, this means if two
-- players simultaneously crash (head on) the first player in the list will
-- have collided, the second will not because players are derezzed (disappear)
-- as soon as they collide.
resolve_collisions : [(Player, (Place, Place), [Place])] -> [Player]
resolve_collisions playerTriples =
    map (\ (p, _, _) -> p) <| resolve_collisions' playerTriples []

-- A trace intersects a lightcycle (bounds) if
intersect : (Place, Place) -> [[Place]] -> Bool
intersect _ _ = False

-- Check if players are within the boundary of the grid and not intersecting
-- any jetwalls.
resolve_collisions' : [(Player, (Place, Place), [Place])]
                        -> [(Player, (Place, Place), [Place])]
                        -> [(Player, (Place, Place), [Place])]
resolve_collisions' checking checked =
    let leftEdge = -(field.width/2)
        rightEdge = (field.width/2)
        topEdge = (field.height/2)
        bottomEdge = -(field.height/2)
        inGrid ((x,y), (u,v)) = x > leftEdge
                                && y > bottomEdge
                                && u < rightEdge
                                && v < topEdge
        trcs = map (\ (_, _, trc) -> trc) (checking++checked)
     in case checking of
          [] -> checked
          (p, bounds, trc) :: toCheck ->
            if inGrid bounds
               && not (intersect bounds trcs)
            then resolve_collisions'
                    toCheck
                    (checked ++ [(p, bounds, trc)])
            else resolve_collisions'
                    toCheck
                    checked

-- This signal represents all players in the game.
players : [Player] -> Signal [Form] -> Signal [Form]
players ps = let dirSignals = map (\ p -> case p of
                                    Player _ _ orientation controls ->
                                      directions orientation (turns controls))
                                  ps
                 plcSignals = zipWith (\ p dirs -> case p of
                                        Player _ place _ _ ->
                                            places place dirs)
                                      ps
                                      dirSignals
                 cycleBound o (x,y) =
                    let fw = toFloat lightcycle.h
                        sw = toFloat lightcycle.w
                     in case o of
                          N -> ((x-sw/2, y     ), (x+sw/2, y+fw  ))
                          E -> ((x     , y-sw/2), (x+fw  , y+sw/2))
                          S -> ((x-sw/2, y-fw  ), (x+sw/2, y     ))
                          W -> ((x-fw  , y-sw/2), (x     , y+sw/2))
                 cycleBounds dirs plcs = cycleBound <~ dirs ~ plcs
                 cycleBoundSignals = zipWith (cycleBounds)
                                             dirSignals
                                             plcSignals
                 trcSignals = zipWith3 (\ p dirs plcs -> case p of
                                         Player _ place _ _ ->
                                           trace jetwall.length place dirs plcs)
                                       ps
                                       dirSignals
                                       plcSignals
                 remaining_ps = let member xs x = case xs of
                                                    [] -> False
                                                    x :: _ -> True
                                                    _ :: ys -> member ys x
                                 in foldp (\ triples prev_ps ->
                                            resolve_collisions
                                              (filter (\ trip -> case trip of
                                                        (p, _, _) ->
                                                          member prev_ps p)
                                                      triples))
                                      ps 
                                      ((zip3 ps) <~ combine cycleBoundSignals
                                                  ~ combine trcSignals)
                 lookup assocs p = case assocs of
                                    (p, result) :: _ -> result
                                    _ :: rassocs -> lookup rassocs p
                 remaining_dirs = (\ r_ps dirs ->
                                    map (lookup (zip ps dirs)) r_ps)
                                    <~ remaining_ps ~ combine dirSignals
                 remaining_plcs = (\ r_ps plcs ->
                                    map (lookup (zip ps plcs)) r_ps)
                                    <~ remaining_ps ~ combine plcSignals
                 remaining_trcs = (\ r_ps trcs ->
                                    map (lookup (zip ps trcs)) r_ps)
                                    <~ remaining_ps ~ combine trcSignals
              in lift2 (++) (players' <~ remaining_ps
                                       ~ remaining_dirs
                                       ~ remaining_plcs
                                       ~ remaining_trcs)

players' : [Player] -> [Orientation] -> [Place] -> [[Place]] -> [Form]
players' ps dirs plcs trcs = concat <| zipWith4 (player)
                                                ps
                                                dirs
                                                plcs
                                                trcs

-- The grid is the combination of all elements in the game that should be
-- displayed.
grid : Signal [Form]
grid = background
        << hud
        << players [Player white (100,100) N (37,39),
                    Player green (50, 100) N (37,39),
                    Player orange (0,100) N (37, 39)]
        <| constant []

-- This signal turns the forms in the grid into an html element that can be
-- displayed.
showGrid : Signal Element
showGrid = (collage (truncate field.width) (truncate field.height))
             <~ grid

-- example use: showPlayer' Color.lightBlue (10, 10) N
showPlayer' : Color -> (Float, Float) -> Orientation -> Form
showPlayer' color (x, y) o =
    let fw = toFloat lightcycle.h
        (xOffset, yOffset, degs) = case o of
           N -> (0, fw / 2, 90)
           E -> (fw / 2, 0, 0)
           S -> (0, -fw / 2, -90)
           W -> (-fw / 2, 0, 180)
     in rect (fw) (toFloat lightcycle.w) 
         |> outlined (solid color)
         |> move (x, y)
         |> move (xOffset, yOffset)
         |> rotate (degrees degs) 


-- example use: showTail Color.lightBlue [(10, 10), (20, 10)]
showTail : Color -> [(Float, Float)] -> Form
showTail color positions = 
    traced { defaultLine |
             width <- jetwall.width
           , color <- color
           } (path positions)

main = showGrid

