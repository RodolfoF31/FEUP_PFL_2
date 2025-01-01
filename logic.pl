:- use_module(library(lists)).
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

valid_moves_from_piece([Board, CurrentPlayer, BoardSize], FromRow, FromCol, Moves) :-
    findall([FromRow, FromCol, ToRow, ToCol],
            (adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
            within_bounds(ToRow, ToCol, BoardSize)),
            Moves).

% bottom_piece_belongs_to_player(+Board, +Row, +Col, +CurrentPlayer)
% Checks if the bottom piece of the stack at (Row, Col) belongs to the CurrentPlayer
bottom_piece_belongs_to_player(Board, Row, Col, CurrentPlayer) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Stack),
    Stack \= [], % Ensure the stack is not empty
    nth1(1, Stack, BottomPiece), % Get the bottom piece (first element in the stack)
    BottomPiece == CurrentPlayer.


is_stack_isolated(Board, BoardSize, Row, Col) :-
    
    % Check top-left diagonal
    (Row1 is Row - 1, Col1 is Col - 1,
     (within_bounds(Row1, Col1, BoardSize) ->
        nth1(Row1, Board, BoardRow1),
        nth1(Col1, BoardRow1, Stack1),
        Stack1 == []
     ; true)),
    % Check top-right diagonal
    (Row2 is Row - 1, Col2 is Col + 1,
     (within_bounds(Row2, Col2, BoardSize) ->
        nth1(Row2, Board, BoardRow2),
        nth1(Col2, BoardRow2, Stack2),
        Stack2 == []
     ; true)),
    % Check bottom-left diagonal
    (Row3 is Row + 1, Col3 is Col - 1,
     (within_bounds(Row3, Col3, BoardSize) ->
        nth1(Row3, Board, BoardRow3),
        nth1(Col3, BoardRow3, Stack3),
        Stack3 == []
     ; true)),
    % Check bottom-right diagonal
    (Row4 is Row + 1, Col4 is Col + 1,
     (within_bounds(Row4, Col4, BoardSize) ->
        nth1(Row4, Board, BoardRow4),
        nth1(Col4, BoardRow4, Stack4),
        Stack4 == []
     ; true)).

% Helper predicate to check if coordinates are within bounds
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