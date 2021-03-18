import Matrix
import SuguruSolver

main = do
    -- print("Cria uma matriz predefinida")
    -- let m = createMatrix [[1,2,3],[4,5,6],[7,8,9]]
    -- print(m)
    {-  Aqui são definidas duas matrizes:
        1. A matriz do tabuleiro SUGURU, com os valores preenchidos sendo seus valores e os não preenchidos sendo zero
        2. A matriz de áreas do tabuleiro, com tuplas representando quais células fazem parte daquela área
    -}
    let suguru = [  [1,0,0,5,0],
                    [0,0,0,0,0],
                    [1,0,2,0,4],
                    [0,0,0,0,0],
                    [0,3,0,0,0]]
    
    let board = [  [(1,1),(1,2),(1,3),(2,1)],
                    [(1,4),(1,5),(2,5),(3,5),(4,5)],
                    [(2,2),(2,3),(3,1),(3,2),(4,1)],
                    [(2,4),(3,3),(3,4),(4,2),(4,3)],
                    [(4,4),(5,1),(5,2),(5,3),(5,4)],
                    [(5,5)]]

    {- Aqui o programa checa se existe alguma área de apenas uma celula, e se existir preenche ela com 1 (seu único valor possível)
    -}
    let suguru2 = fillAreas1 board suguru
    -- print(suguru2)
    -- print()
    -- print()
    {- O programa então cria uma nova matriz auxiliar: a matriz de possibilidades. O formato tem a mesma estrutura da de áreas, para
        que seja possível correlacionar os endereços das células das áreas com seus possíveis valores.
        Ex: se o elemento m3x3 da matriz de áreas é (5,7), então a celula s5x7 do tabuleiro pode ter os valores da lista
        l3x3 da matriz de possibilidades.

        Nesse ponto ainda existem valores que não são uma possibilidade nas listas. O que a função faz é pegar o tamanho
        da área e colocar como possibilidades os bounds do tamanho para cada célula. Ex: se a área tem 5 quadrados,
        então seus elementos estão em [1,5]. Mesmo que ja exista o elemento 4 nessa área;
    -}
    let possibilities = getPossibilitiesMatrix board suguru2
    -- print(possibilities)
    {-
    [
        [[1],[1,2,3,4],[1,2,3,4],[1,2,3,4]],
        [[5],[1,2,3,4,5],[1,2,3,4,5],[4],[1,2,3,4,5]],
        [[1,2,3,4,5],[1,2,3,4,5],[1],[1,2,3,4,5],[1,2,3,4,5]],
        [[1,2,3,4,5],[2],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]],
        [[1,2,3,4,5],[1,2,3,4,5],[3],[1,2,3,4,5],[1,2,3,4,5]],
        [[1]]
    ]
    -}
    -- print()
    -- print()

    {- Agora é feita a correção da função acima. Usando do mesmo exemplo: uma área de 5 quadrados ja tem o elemento 4, então é retirada
    essa possibilidade das listas de possibilidades das outras celulas dessa área. Supondo que essa área esteja no seguinte formato:
    [0,0,4,0,0], suas possibilidades serão agora -> [ [1,2,3,5], [1,2,3,5], [4], [1,2,3,5], [1,2,3,5] ]
    -}
    let possibilites2 = clearPossibilities possibilities
    --print(possibilites2)
    {-
    [
        [[1],[2,3,4],[2,3,4],[2,3,4]],
        [[5],[1,2,3],[1,2,3],[4],[1,2,3]],
        [[2,3,4,5],[2,3,4,5],[1],[2,3,4,5],[2,3,4,5]],
        [[1,3,4,5],[2],[1,3,4,5],[1,3,4,5],[1,3,4,5]],
        [[1,2,4,5],[1,2,4,5],[3],[1,2,4,5],[1,2,4,5]],
        [[1]]
    ]
    -}
    -- print()
    -- print()
    {- TO DO -> COLOCAR AQUI UMA FUNÇÃO QUE PASSA NOVAMENTE POR TODAS AS POSSIBILIDADES E SE HOUVER UMA LISTA COM APENAS UMA POSSIBILIDADE,
    JA COLOCAR ELA NO TABULEIRO. ISSO PODE ECONOMIZAR UM TEMPINHO NA PRÓXIMA FUNÇÃO
    -}
    let possibilites_final = checkSurroundings board possibilites2 suguru2 0
    print(possibilites_final)

    {-
        [
            [[1],[2,3,4],[2,3,4],[2,3,4]],
            [[5],[1,2,3],[1,2,3],[4],[2,3]],
            [[3,4,5],[3,4],[1],[3,4,5],[2,4,5]],
            [[1,3],[2],[1,3,5],[4,5],[1,4,5]],
            [[5],[1,2,4,5],[3],[1,2,4,5],[2,4,5]],
            [[1]]]
    -}

    {- Aqui começa a ser feita as operações fora de áreas.
        1. Pegaremos célula a célula do tabuleiro
            1.1 Se ela estiver preenchida com zero, checaremos a localização dela para vermos quais células ao redor dela existem, e 
            checaremos essas celulas para ver se alguma já esta preenchida. Caso encontremos uma preenchida, vamos tirar o valor dessa
            preenchida da lista de possibilidades da célula analisada.
            1.2 Se ela estiver preenchida com qualquer coisa diferente de zero, vamos ver quais células ao redor dela existem,
            e dessas vamos tirar da lista de possibilidades o valor da célula analisada
    -}

    {- TO DO -> COLOCAR AQUI UMA FUNÇÃO QUE PASSA NOVAMENTE POR TODAS AS POSSIBILIDADES E SE HOUVER UMA LISTA COM APENAS UMA POSSIBILIDADE,
    JA COLOCAR ELA NO TABULEIRO. ISSO PODE ECONOMIZAR UM TEMPINHO NA PRÓXIMA FUNÇÃO
    -}

    {-
    AQUI PODEMOS OU REPETIR OS PASSOS ANTERIORES PARA TENTAR MATAR MAIS ALGUMAS CELULAS COM VALOR UNICO OU JA COMEÇAR O TENTATIVA E ERRO
    -}