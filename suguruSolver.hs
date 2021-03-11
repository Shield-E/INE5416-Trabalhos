module SuguruSolver(fillAreas1) where

    import Matrix

    fillAreas1 :: [[(Int,Int)]] -> [[Int]] -> [[Int]]
    fillAreas1 board suguru = fillAreas1Aux board suguru

    fillAreas1Aux :: [[(Int,Int)]] -> [[Int]] -> [[Int]]
    fillAreas1Aux [] m = m
    fillAreas1Aux (x:y) (a:b) = 
        if length x == 1 then
            fillAreas1Aux y (put (a:b) (x!!0) 1)
        else
            fillAreas1Aux y (a:b)