import Matrix

main = do
    -- print("Cria uma matriz predefinida")
    -- let m = createMatrix [[1,2,3],[4,5,6],[7,8,9]]
    -- print(m)
    print("Cria uma matrix 3x3 de zeros")
    let z = zeros 3 3
    print(z)
    print("Coloca o elemento 1 no local 2x2 da matriz de zeros")
    let pos = (2,2)
    let z2 = put z pos 1
    print(z2)