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

---------------------------------------------------------------------------------------------------------------------------------------------

    {- Chama o checkSur area por área
    checkSurroundings ::    board     -> possibilities ->   suguru   -> loop count -> possibilities-}
    checkSurroundings :: [(Int, Int)] ->    [[[Int]]]  ->  [[[Int]]] ->     Int    ->    [[[Int]]]
    checkSurroundings board possibilities suguru loop = --indexArea vai ser zero na chamada do checkSur
        if loop == (length board) then []
        else
            (checkSur board possibilities suguru loop 0) : (checkSurroundings board possibilities suguru (loop+1))

    -- tem que retornar as possibilities inteiras, não só a da área
    checkSur board poss suguru indexArea loop = 
        if loop == (length (board!!indexArea)) then [] -- se tiver finalizado as células da área, pare
        else
            if (suguru!!(fst (board!!indexArea)!!loop))!!(snd (board!!indexArea)!!loop) == 0 then --se no tabuleiro a célula dessa área conter zero
                {- se o elemento suguru(x,y) (onde x e y vem dos elementos das áreas) for == 0, pegaremos
                os elementos em volta dele e caso algum deles seja diferente de zero vamos retirar
                essa possibilidade do elemento 0
                -}

                {-                                    (em poss tamb)   (em poss tamb)
                                                    #area na board| #elemento na area| board | suguru | possibilities-}
                removeAroundPossibilitiesFromElement     indexArea          loop       board   suguru   possibilities
            else -- a celula nao tem zero, entao vamos remover o valor dela das possibilidades ao redor dela


    {-
    Element places example
    ------------------------------------
    |           |          |           |
    |upper left |  upper   |upper right|
    |           |  central |           |
    ------------------------------------
    |           |          |           |
    |left wall  |  central |right wall |
    |    -      |     -    |      -    |
    ------------------------------------
    |     -     |     -    |     -     |
    |   lower   |   lower  |  lower    |
    |   left    |  central |   right   |
    ------------------------------------

    -}
    removeAroundPossibilitiesFromElement
    removeAroundPossibilitiesFromElement idxArea idxElem board suguru poss = 
        -- row -> (fst ((board!!idxArea)!!idxElem))
        -- col -> (snd ((board!!idxArea)!!idxElem))
        -- board size -> (length suguru) (NxN)
        if (fst ((board!!idxArea)!!idxElem)) > 0 then -- row > 0
            if (fst ((board!!idxArea)!!idxElem)) < (length suguru) then  -- row < size
                if (snd ((board!!idxArea)!!idxElem)) > 0 then -- col > 0
                    if (snd ((board!!idxArea)!!idxElem)) < (length suguru) then -- col < size
                        removeFromCentral (fst ((board!!idxArea)!!idxElem)) (snd ((board!!idxArea)!!idxElem)) idxArea idxEleme board poss suguru
                    else
                        removeFromRightWall
                else
                    removeFromLetfWall
            else
                if (snd ((board!!idxArea)!!idxElem)) > 0 then  -- col > 0
                    if (snd ((board!!idxArea)!!idxElem)) < (length suguru) then -- col < size
                        removeFromLowerCentral
                    else
                        removeFromLowerRight
                else
                    removeFromLowerLeft
        else
            if (snd ((board!!idxArea)!!idxElem)) > 0 then  -- col > 0
                if (snd ((board!!idxArea)!!idxElem)) < (length suguru) then  -- col < size
                    removeFromUpperCentral
                else
                    removeFromUpperRight
            else
                removeFromUpperLeft

    {-
        area -> number of the area in the board matrix.
        element -> index of the element inside the area array
        row -> row of the cell in the suguru
        col -> col of the cell in the suguru
        suguru -> suguru board to check the other values
    -}
    removeFromCentral row col idxArea idxElem board poss suguru =
        -- value on the upper left
        if length (getValueFrom (row-1) (col-1) board) == 1 then
            removeFromPossibilities (getValueFrom (row-1) (col-1) board) idxArea idxElem poss 0 0
        -- value on the upper
        if length (getValueFrom (row-1) (col) board) == 1 then
            removeFromPossibilities (getValueFrom (row-1) (col) board) idxArea idxElem poss 0 0
        -- value on the upper right
        if length (getValueFrom (row-1) (col+1) board) == 1 then
                removeFromPossibilities (getValueFrom (row-1) (col+1) board) idxArea idxElem poss 0 0
        -- value on the left
        if length (getValueFrom (row) (col-1) board) == 1 then
                removeFromPossibilities (getValueFrom (row) (col-1) board) idxArea idxElem poss 0 0
        -- value on the right
        if length (getValueFrom (row-1) (col+1) board) == 1 then
                removeFromPossibilities (getValueFrom (row-1) (col+1) board) idxArea idxElem poss 0 0
        -- value on the lower left
        if length (getValueFrom (row+1) (col-1) board) == 1 then
                removeFromPossibilities (getValueFrom (row+1) (col-1) board) idxArea idxElem poss 0 0
        -- value on the lower
        if length (getValueFrom (row+1) (col) board) == 1 then
                removeFromPossibilities (getValueFrom (row+1) (col) board) idxArea idxElem poss 0 0
        -- value on the lower right
        if length (getValueFrom (row+1) (col+1) board) == 1 then
                removeFromPossibilities (getValueFrom (row+1) (col+1) board) idxArea idxElem poss 0 0
    
    getValueFrom row col board = (board!!row)!!col

    removeFromPossibilities value idxArea idxElem (a:b:c) {-poss-} loopArea loopElem = 
        if loopArea == idxArea then
            a : (delete value b!!idxElem) : c
        else
            a : removeFromPossibilities value idxArea idxElem (b:c) {-poss-} loopArea loopElem
