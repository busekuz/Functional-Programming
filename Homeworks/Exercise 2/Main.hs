module MainModule where

import Bonus

main = do 
        putStrLn "Enter cards:"
        cards <- readCards []
        putStrLn (show cards)
        putStrLn "Enter moves:"
        moves <- readMoves []
        putStrLn (show moves)
        putStrLn "Enter goal:"
        line <- getLine
        let goal = read line :: Int
        let score = runGame cards moves goal
        putStrLn ("Score: " ++ show score)
