module Matrix(  Matrix, Position,
                newMatrix, newPos, zeros, put, changeOnList, transpose,
                mainDiagonal, secondDiagonal)
                where
    data Matrix = Matrix [[Int]]
    data Position = Position(Int, Int)

    newMatrix :: [[Int]] -> Matrix
    newMatrix m = Matrix m

    newPos :: Int -> Int -> Position
    newPos x y = Position(x,y)

    zeros :: Int -> Int -> [[Int]]
    zeros 0 _ = []
    zeros rows cols = (take cols (repeat 0)) : zeros (rows-1) cols

    put :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
    put m (r,c) n = putAux m ((r-1), (c-1)) n

    putAux :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
    putAux (a:b) (r,c) n
                | r == 0 = (changeOnList c n a) : b
                | otherwise = a : putAux b ((r-1),c) n 
    
    get :: (Int, Int) -> [[Int]] -> Int
    get (r,c) m = (m!!r)!!c

    isSquare :: [[Int]] -> Bool
    isSquare m = 
        if length (m!!0) == length m then True
        else False

    changeOnList :: Int -> a -> [a] -> [a]
    changeOnList _ _ [] = []
    changeOnList n newVal (x:xs)
                            | n == 0 = newVal : xs
                            | otherwise = x : changeOnList (n-1) newVal xs
    
    transpose :: [[Int]] -> [[Int]]
    transpose ([]:_) = []
    transpose m = (map head m) : transpose (map tail m)

    mainDiagonal :: [[Int]] -> Int
    mainDiagonal m = mainDiagonalAux m ((length m)-1)

    mainDiagonalAux :: [[Int]] -> Int -> Int
    mainDiagonalAux m 0 = ((m!!0)!!0)
    mainDiagonalAux m n = ((m!!n)!!n) + (mainDiagonalAux m (n-1))

    secondDiagonal :: [[Int]] -> Int
    secondDiagonal m = secondDiagonalAux m ((length m)-1) 0

    secondDiagonalAux :: [[Int]] -> Int -> Int -> Int
    secondDiagonalAux m 0 c= ((m!!0)!!c)
    secondDiagonalAux m l c = ((m!!l)!!c) + (secondDiagonalAux m (l-1) (c+1))