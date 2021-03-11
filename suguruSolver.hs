module SuguruSolver(fillAreas1, getPossibilitiesArea, getPossibilitiesMatrix, clearPossibilities) where

    import Matrix
    import Data.List -- (delete 10 [0,0,10] removes the 10 ; delete 2 [0,1,10] does nothing without error)

    {- Fill all the areas that have size 1 with the number 1 (just that possibility)-}
    fillAreas1 :: [[(Int,Int)]] -> [[Int]] -> [[Int]]
    fillAreas1 [] m = m
    fillAreas1 (x:y) (a:b) = 
        if length x == 1 then
            fillAreas1 y (put (a:b) (x!!0) 1)
        else
            fillAreas1 y (a:b)
    
    {- returns a 3d matrix with all the possibilites of numbers for each cell, using the size of the area;
    -> calls getPossibilitiesArea for each area of the board
    !!! not final possibilities, only fills with array of [1 -> size], but doesnt see if any other cell is already filled
    getPossibilitiesMatrix :: board          -> suguru  -> R: matriz de possibilidades-}
    getPossibilitiesMatrix :: [[(Int, Int)]] -> [[Int]] -> [[[Int]]]
    getPossibilitiesMatrix [] _ = []
    getPossibilitiesMatrix (x:y) matriz =
        (getPossibilitiesArea x matriz ((length x)-1) (length x)) : (getPossibilitiesMatrix y matriz)

    {-returns a 2d matrix, containing an array of possibilites for each cell in the received area
    !!! same of the getPossibilitiesMatrix applies here
    getPossibilitiesArea :: board        -> suguru  -> loopCount -> tamanhoarea -> R: matriz de possibilidades -}
    getPossibilitiesArea :: [(Int, Int)] -> [[Int]] -> Int       -> Int          -> [[Int]]
    getPossibilitiesArea _ _ (-1) _ = []
    getPossibilitiesArea (l1:l2) matriz n tamanhoArea = 
        if (matriz!!((fst l1)-1))!!((snd l1)-1) == 0 then
            -- preenche com uma lista de [1..length area]
            [1..tamanhoArea] : (getPossibilitiesArea l2 matriz (n-1) tamanhoArea)
        else
            -- coloca uma lista com o própio elemento na lista
            [(matriz!!((fst l1)-1))!!((snd l1)-1)] : (getPossibilitiesArea l2 matriz (n-1) tamanhoArea)
    

    {- runs through each area, removing from the possibilities the numbers that
    are already present in that area
    clearPossibilities :: possibilites  -> possibilities -}
    clearPossibilities ::   [[[Int]]]   ->   [[[Int]]]
    clearPossibilities [] = []
    clearPossibilities (a:b) = (clearArea a (length a) 0) : (clearPossibilities b)

    {-
    clearArea :: possibilities of area -> area size  -> loop -> possibilites-}
    clearArea ::      [[Int]]         ->     Int     ->  Int ->  [[Int]]
    clearArea pos size loop =
        if loop == size then pos
        else 
            if length (pos!!loop) == 1 then
                -- if the possibilities for that cell is only one (already correct) then remove this number from all the OTHER cells
                clearArea (removeNumberFromOtherCells pos ((pos!!loop)!!0) loop size 0) size (loop+1)
            else
                -- if not, keep this cell intact and continue
                clearArea pos size (loop+1)
    
    {-
    removeNumberFromOtherCells ::  cells  -> number -> index to dont change -> area size -> loop count -> cells  -}
    removeNumberFromOtherCells :: [[Int]] ->   Int  ->          Int         ->    Int    ->     Int    -> [[Int]]
    removeNumberFromOtherCells area n idx size count = 
        if count == size then []
        else
            if count == idx then
                area!!count : removeNumberFromOtherCells area n idx size (count+1)
            else
                (delete n (area!!count)) : (removeNumberFromOtherCells area n idx size (count+1))

    {-
    checkSurroundings ::    board     -> possibilities -> possibilities-}
    checkSurroundings :: [(Int, Int)] ->    [[[Int]]]  ->    [[[Int]]]
    checkSurroundings