import check
import matrix
import suguruSolver

type numSuguru = Int
type coordX = Int
type coordY = Int
type resultMatrix = [[Int]]
type resultPossibility = [[[Int]]]
type resultTrial = (numSuguru, coordX, coordY, resultMatrix, resultPossibility)

coord :: [[Int]] -> Int -> Int -> (Int,Int)
coord matrix i j =
  if(matrix!!i!!j) == 0 then
    (i,j)
  else
    if i < (length matrix) then
      if j < (length matrix!!0) then
        coord matrix i (j+1)
      else
        coord matrix (i+1) 0
    else
      (-1, -1)

cabeca :: [t] -> t
cabeca (a:_) = a

cauda :: [t] -> t
cauda (_:b) = b

validate :: [[Int]] -> [[Int]] -> [[[Int]]] -> Bool
validate previous suguru possibility = do
  let (c,a,b,e,d) = trial previous suguru possibility
  let b1 = check 0 0 e
  let m1 = e
  if (b1) then
    True
  else
    m1!!a!!b = 0
    validate previous m1 d

{-       1 ou 0 -> matriz anterior -> matriz suguru em questao -> matriz possibilidade-}
trial :: [[Int]] -> [[Int]] -> [[[Int]]] ->  resultTrial
trial previous suguru possibility = do
  let coordZero = coord suguru 0 0
  let a = coordZero (a,_)
  let b = coordZero (_,b)

  let c = cabeca (possibility!!a!!b)
  let d = possibility
  (d!!a!!b) = cauda (d!!a!!b)

  let e = suguru
  (e!!a!!b) = c
  trial (c,a,b,e,d)
