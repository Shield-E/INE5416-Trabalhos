{- O método check verifica se exitem casas próximas com elementos iguais -}
{- Os métodos auxiliares apenas servem para manter o método check pequeno  -}
{- Nenhum método auxiliar deve ser modificado -}

module Check (checkAux1,checkAux2,checkAux3,check) where

checkAux1 :: Int -> Int -> [[Int]] -> Bool
checkAux1 lin col matrix =
  if (matrix!!lin!!col /= 0) then
    if ((matrix!!lin!!col) == (matrix!!lin!!(col+1))) then
      False
    else if (col > 0) && ((matrix!!lin!!col) == (matrix!!(lin+1)!!(col-1))) then
      False
    else if ((matrix!!lin!!col) == (matrix!!(lin+1)!!(col))) then
      False
    else if ((matrix!!lin!!col) == (matrix!!(lin+1)!!(col+1))) then
      False
    else
      check lin (col+1) matrix
  else
    check lin (col+1) matrix

checkAux2 :: Int -> Int -> [[Int]] -> Bool
checkAux2 lin col matrix =
  if (matrix!!lin!!col /= 0) then
    if ((matrix!!lin!!col) == (matrix!!(lin+1)!!(col))) then
      False
    else if ((matrix!!lin!!col) == (matrix!!(lin+1)!!(col-1))) then
      False
    else
      check (lin+1) 0 matrix
  else
    check (lin+1) 0 matrix

checkAux3 :: Int -> Int -> [[Int]] -> Bool
checkAux3 lin col matrix =
  if (matrix!!lin!!col /= 0) then
    if (col < ((length (matrix!!0))-1)) then
      if ((matrix!!lin!!col) == (matrix!!lin!!(col+1))) then
        False
      else
        check lin (col+1) matrix
    else
      True
  else
    check lin (col+1) matrix

check :: Int -> Int -> [[Int]] -> Bool
check lin col matrix =
  if lin < ((length matrix)-1) then
    if (col < ((length (matrix!!0))-1)) then
      if (matrix!!lin!!col /= 0) then
        checkAux1 lin col matrix
      else
        check lin (col+1) matrix
    else if (col == ((length (matrix!!0))-1)) then
      if (matrix!!lin!!col /= 0) then
        checkAux2 lin col matrix
      else
        check lin (col+1) matrix
    else
      if (matrix!!lin!!col /= 0) then
        check (lin+1) 0 matrix
      else
        check (lin+1) 0 matrix
  else if lin == ((length matrix)-1) then
    if (matrix!!lin!!col /= 0) then
      checkAux3 lin col matrix
    else
      check lin (col+1) matrix
  else
    True
