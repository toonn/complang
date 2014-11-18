-- Modify the main program below, so that it shows the text "Time:
-- 12345678" instead of "Hello World!". The value 12345678 should be
-- the current value of the time signal.

time : Signal Float
time = every second

main : Signal Element
main = lift asText time
