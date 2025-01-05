# Turma 7 Byte_01


| Name             | Number    | E-Mail             | Contribution |
| ---------------- | --------- | ------------------ | -------------- |
| António Ferreira         | 202108834 | up202108834@up.pt              | 50%
| José Ferreira        | 202108836 | up202108836@up.pt                | 50%


## Instalação e Execução
* Executar Sicstus Prolog
* File  -> Consult -> game.pl
* Na consola do sicstus: play.

## Byte  - Descrição do jogo
  

Byte é um jogo estratégico para dois jogadores jogado num tabuleiro 8x8 ou 10x10, onde apenas as casas escuras são utilizadas. Os jogadores alternam turnos movendo ou fundindo stacks de peças no tabuleiro. As peças  brancas jogam primeiro.

O objetivo do jogo é ser o primeiro a conquistar dois pontos. Um ponto é obtido ao formar uma stack de exatamente 8 peças, onde a última peça no topo pertence ao jogador. Assim que a stack de 8 peças é formada, ela é removida do tabuleiro, e o ponto vai para o jogador cuja peça estava no topo. O jogador que conquistar primeiro 2 pontos vence o jogo.

### Regras de movimentos
* Movimentos Básicos:

Se uma stack não estiver adjacente a nenhuma outra no tabuleiro, e se o jogador possuir a peça na base dessa stack, ele pode deslizar a stack inteira para uma casa diagonal adjacente. A stack deve ser movida na direção da stack mais próxima, medida pelo número de movimentos necessários para alcançá-la.

* Merge de stacks:

1.  Se duas stacks estiverem adjacentes, um jogador pode mover uma parte da sua stack A para a stack B. O jogador escolhe uma peça sua (em qualquer nível) e move-a junto com todas as peças acima dela para a stack B. As regras para a fusão são:

2. A peça movida deve ir para um nível superior. Não é permitido movê-la para o mesmo nível ou para um nível inferior.
Não se pode formar uma stack com mais de 8 peças.

* Restrições adicionais:

1. Se houver stacks adjacentes, nenhuma delas pode ser movida para uma casa vazia.

2. Se houver qualquer movimento possível, o jogador é obrigado a realizá-lo, mesmo que beneficie o adversário.

3. Se não houver movimentos possíveis, o jogador deve passar a vez.

## Game Logic

### Game Configuration  representation

A **configuração do jogo** em BYTE determina como o jogo será configurado e jogado. Ela inclui parâmetros essenciais que definem as condições iniciais e as interações entre os jogadores. A configuração é passada como argumento para o predicado `initial_state/2`, que cria o estado inicial do jogo.

#### Informações Necessárias para Representar a Configuração do Jogo

1. **Modo**: Especifica o modo de jogo. Pode ser:
   - `'Pvp'`: Jogador vs. Jogador.
   - `'Pvc'`: Jogador vs. Computador.
   - `'Cvc'`: Computador vs. Computador.

2. **BoardSize**: Define o tamanho do tabuleiro. BYTE suporta tabuleiros de 8x8 ou 10x10.

3. **Player1Type**: Define o tipo do jogador 1:
   - `'human'` para jogador humano.
   - `'computer'` para jogador controlado pelo computador.

4. **Player2Type**: Define o tipo do jogador 2, com os mesmos valores que `Player1Type`.

#### Representação Interna

O parâmetro `GameConfig` é representado como uma lista com as opções de configuração:  
`GameConfig = [Mode, BoardSize, Player1Type, Player2Type]`

O parâmetro `GameState`, inicializado com base em `GameConfig`, contém as informações necessárias para gerenciar o jogo:
- `Board`: Representa o estado atual do tabuleiro.
- `1`: Representa o jogador atual (1 para o Jogador 1, 2 para o Jogador 2).
- `BoardSize`: O tamanho do tabuleiro.
- `0`: Pontuação inicial do Jogador 1.
- `0`: Pontuação inicial do Jogador 2.
- `Player1Type`: O tipo do Jogador 1.
- `Player2Type`: O tipo do Jogador 2.

O `GameState` é representado como:  
`GameState = [Board, CurrentPlayer, BoardSize, Player1Score, Player2Score, Player1Type, Player2Type]`

#### Uso em `initial_state/2`

O predicado `initial_state/2` inicializa o estado do jogo com a configuração fornecida:
- **Inicialização do Tabuleiro**: `initialize_board(BoardSize, Board)` cria a estrutura do tabuleiro de acordo com o tamanho especificado.
- **Laço de Jogo**: Dependendo do modo, `initial_state/2` chama diferentes predicados de loop de jogo:
  - `game_loop/1` para o modo Jogador vs. Jogador.
  - `game_loop_pvc/1` para o modo Jogador vs. Computador.
  - `game_loop_cvc/1` para o modo Computador vs. Computador.

O estado inicial do jogo e a configuração são exibidos para referência. Essa configuração garante que o jogo comece com os parâmetros, tamanho do tabuleiro e tipos de jogadores definidos pelo usuário.

### Internal Game State Representation

O tabuleiro no BYTE é representado como uma lista bidimensional, onde cada sublista representa uma linha do tabuleiro.

- Uma **lista vazia** (`[]`) indica uma posição vazia.
- **1** representa uma stack com uma peça branca no topo.
- **-1** representa uma stack com uma peça preta no topo.

```prolog
Possiveis estados de jogo:
- Inicial :
[
    [[], [], [], [], [], [], [], []],
    [[], [-1], [], [-1], [], [-1], [], [-1]],
    [[1], [], [1], [], [1], [], [1], []],
    [[], [-1], [], [-1], [], [-1], [], [-1]],
    [[1], [], [1], [], [1], [], [1], []],
    [[], [-1], [], [-1], [], [-1], [], [-1]],
    [[1], [], [1], [], [1], [], [1], []],
    [[], [], [], [], [], [], [], []]
].

- Intermédio :
[
    [[], [], [], [], [], [], [], []],
    [[], [], [], [-1], [], [], [], []],
    [[], [], [1], [], [1], [], [1], []],
    [[], [], [], [-1], [], [-1], [], [-1]],
    [[], [], [1], [], [1], [], [], []],
    [[], [-1], [], [-1], [], [], [], [-1]],
    [[], [], [1], [], [], [], [1], []],
    [[], [], [], [], [], [], [], []]
].

- Final:
[
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [-1], [], [], [], []],
    [[], [], [], [], [1], [], [], []],
    [[], [], [], [-1], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []]
].
```
#### Representação dos Jogadores

O jogador tem os seguintes estados possíveis:
- **0**: jogador humano.
- **1**: Bot de nível 1.
- **2**: Bot de nível 2.

Na visualização do tabuleiro:
- As peças do Jogador 1 (Branco) são representadas por `'O'`.
- As peças do Jogador 2 (Preto) são representadas por `'X'`.

Isso é definido pelo predicado `player_char/2`:
```prolog
player_char(0, '').  
player_char(1, 'O').  
player_char(-1, 'X'). 
```

## Visualização da  representação do jogo

Ao iniciar o jogo com o predicado `play.`, o jogador vê o seguinte menu:

![Menu](Menu.png)


Para escolher um modo de jogo, o jogador escreve o número associado ao  modo que quer jogar e prime  'Enter'

Após selecionar um dos modos de jogo, é apresentado ao  jogador outro menu para escolher o tamanho do  tabuleiro (8x8 ou 10x10) e  no caso de o modo de jogo envolver um Bot, é apresentado um menu para escolher a dificuldade do Bot (Nivel 1 ou Nivel 2)

Assim que o  jogo é iniciado é apresentado o tabuleiro:

![Tabuleiro](Tabuleiro.png)

Devido à natureza do jogo, nós decidimos que seria melhor mostrar a informação de cada stack visto que não é possivel num display 2D ver todas as peças que constituem a stack e  seria muito dificil memorizar todas as peças de todas as stacks.

Ao pedir um input ao jogador, caso o input não seja válido, é apresentada uma mensagem de erro e é pedido o input outra vez.

### Execução de jogadas

Para realizar uma jogada, duas condições devem ser verificadas:
1. As coordenadas fornecidas devem ser válidas.
2. A stack selecionada deve pertencer ao jogador, e a jogada deve seguir as regras de movimentação (basic move ou merge).

As coordenadas e o índice de uma peça são combinados em uma lista de 5 argumentos (o índice é 0 para basic moves). A validade da jogada é verificada com o predicado `valid_moves/2`. Se válida, o predicado `move/3` executa a jogada e atualiza o `GameState`.

### Final do jogo

No BYTE, não há empates. O predicado `game_over/2` verifica se um dos jogadores acumulou 2 pontos. Se sim, o jogo termina e o vencedor é anunciado.

### Jogada do Computador

Dois níveis de dificuldade foram implementados:
- **Nível 1**: Escolhe uma jogada aleatória usando o predicado `random` dos valid_moves.
- **Nível 2**: Para o nível dois, implementámos um predicado `value` que avalia o estado do jogo e retorna um valor numérico que representa quão favorável ou desfavorável o jogo está para o jogador atual. Esta função baseia-se em várias heurísticas. A avaliação considera os seguintes fatores: o número de pilhas completas conquistadas pelo jogador, o número de pilhas controladas pelo jogador (com a sua peça no topo), o número total de pilhas que contêm pelo menos uma peça do jogador, a soma das alturas das pilhas controladas pelo jogador e a proximidade das pilhas do jogador de atingir a altura máxima de 8 (necessária para ganhar a pilha). Estes fatores também são calculados para o adversário, e as diferenças entre as métricas do jogador e do adversário contribuem para o valor final. Para decidir o próximo movimento, simulamos todos os movimentos válidos, calculamos o valor correspondente a cada estado resultante e escolhemos o movimento que nos coloca em vantagem.


This Prolog function, value/3, evaluates the quality of a game state for a given player in the board game Byte. It computes a numerical value based on various heuristics to assess the player's advantage relative to their opponent. The evaluation considers the following factors: the number of stacks the player has completed (win stacks), the number of stacks the player controls with their piece on top, the total number of stacks containing at least one of the player's pieces, the cumulative height of stacks controlled by the player, and the proximity of the player's stacks to the maximum height of 8 (which would secure a win). These factors are calculated similarly for the opponent, and the differences between the player’s and opponent's metrics contribute to the final evaluation score. Higher values favor the player, while lower values indicate an advantageous state for the opponent. The evaluation incorporates positional and strategic considerations, aiming to guide AI decisions effectively.
### Conclusões

A implementação do predicado `valid_moves/2` foi um dos maiores desafios devido à complexidade das regras do jogo. Isso consumiu grande parte do tempo de desenvolvimento, deixando pouco espaço para refatoração e organização final do código.