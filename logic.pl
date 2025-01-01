% game_over(+GameState, -Winner)
% Determines if the game is over and declares the winner based on majority 8-piece stacks.
game_over(GameState, Winner) :-
    GameState = [Board, _, _], % Extract the board from the game state
    findall(Player, (member(Row, Board), member(Stack, Row), is_winning_stack(Stack, Player)), Winners),
    count_majority(Winners, Winner).

% Helper predicate to check if a stack is a winning stack
is_winning_stack(Stack, Player) :-
    length(Stack, 8), 
    last(Stack, Player).

% count_majority(+Winners, -Winner)
% Determines the player with the majority of the winning stacks
count_majority(Winners, Winner) :-
    count_occurrences(Winners, 1, Count1),
    count_occurrences(Winners, 2, Count2),
    ( Count1 > Count2 -> Winner = 1
    ; Count2 > Count1 -> Winner = 2
    ; fail 
    ).

% count_occurrences(+List, +Element, -Count)
% Counts occurrences of Element in List
count_occurrences(List, Element, Count) :-
    include(=(Element), List, Filtered),
    length(Filtered, Count).


% BASIC MOVES

get_piece(Board, Row, Col, Piece,CurrentPlayer) :-
   
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece),
    %format('Piece at (~d, ~d): ~w~n', [Row, Col, Piece]),
    (Piece = [] -> 
        ( fail); 
        (( Piece = [CurrentPlayer] ; Piece =[CurrentPlayer|_] ) -> 
            ( true); 
            (fail))).

player_piece(Board, Player, Row, Col) :-
    get_piece(Board, Row, Col, Piece, Player),
    Piece \= [].


% valid_moves(+GameState, -Moves)
valid_moves([Board, CurrentPlayer, BoardSize], Moves) :-
    findall([Row, Col], player_piece(Board, CurrentPlayer, Row, Col), Positions),
    findall([FromRow, FromCol, ToRow, ToCol],
            (member([FromRow, FromCol], Positions),
            valid_individual_move(FromRow, FromCol, ToRow, ToCol, BoardSize)),
            Moves),
    group_moves_by_from_position(Moves, MovesDict,UniqueFromPositions),
    filter_stack_moves(Board, UniqueFromPositions, MovesDict, ValidMoves),
    write('Generated Moves: '), write(UniqueFromPositions), nl.

filter_stack_moves().

    

is_not_empty(Board, [ToRow, ToCol]) :-
    nth1(ToRow, Board, BoardRow),
    nth1(ToCol, BoardRow, Stack),
    Stack \= [],
    format('Position (~d, ~d) is not empty: ~w~n', [ToRow, ToCol, Stack]).


group_moves_by_from_position(Moves, MovesDict,UniqueFromPositions) :-
    findall([FromRow, FromCol], member([FromRow, FromCol, _, _], Moves), FromPositions),
    sort(FromPositions, UniqueFromPositions),
    write('Unique From Positions: '), write(UniqueFromPositions), nl,
    findall(FromPos-ToPositions,
            (member([FromRow, FromCol], UniqueFromPositions),
             findall([ToRow, ToCol],
                     member([FromRow, FromCol, ToRow, ToCol], Moves),
                     ToPositions),
             FromPos = [FromRow, FromCol],
             write('From Position: '), write(FromPos), write(' To Positions: '), write(ToPositions), nl),
            MovesDict).

valid_individual_move(FromRow, FromCol, ToRow, ToCol, BoardSize) :-
    adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
    within_bounds(ToRow, ToCol, BoardSize).



within_bounds(Row, Col, BoardSize) :-
    Row >= 1, Row =< BoardSize,
    Col >= 1, Col =< BoardSize.

adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol) :-
    (ToRow is FromRow - 1, ToCol is FromCol - 1); % Top-left
    (ToRow is FromRow - 1, ToCol is FromCol + 1); % Top-right
    (ToRow is FromRow + 1, ToCol is FromCol - 1); % Bottom-left
    (ToRow is FromRow + 1, ToCol is FromCol + 1). % Bottom-right



between(Low, High, Value) :-
    Low =< High,
    between_(Low, High, Value).

between_(Low, High, Low).
between_(Low, High, Value) :-
    Low < High,
    NextLow is Low + 1,
    between_(NextLow, High, Value).