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
    findall([Row, Col], (nth1(Row, Board, BoardRow), nth1(Col, BoardRow, Piece), Piece \= []), AllPieces),
    %divide postions into isolated and non isolated
    findall([Row, Col], (member([Row, Col], Positions), is_stack_isolated(Board, BoardSize, Row, Col)), IsolatedPositions),
    findall([Row, Col], (member([Row, Col], AllPieces), \+ is_stack_isolated(Board, BoardSize, Row, Col)), NonIsolatedPositions),

    write('Isolated Positions: '), write(IsolatedPositions), nl,
    write('Non Isolated Positions: '), write(NonIsolatedPositions), nl,
    %return the isolated
    %return the non isolated
    %find the correct moves for isolated
    findall([FromRow, FromCol, ToRow, ToCol],
            (member([FromRow, FromCol], IsolatedPositions),
             isolated_moves(FromRow, FromCol, ToRow, ToCol, BoardSize)),
            IsolatedMoves),
    findall([FromRow, FromCol, ToRow, ToCol,StackPosition],
            (member([FromRow, FromCol], NonIsolatedPositions),
             non_isolated_moves(FromRow, FromCol, ToRow, ToCol,StackPosition, [Board, CurrentPlayer, BoardSize])),
            NonIsolatedMoves),
    write('Generated Non-Isolated Moves: '), write(NonIsolatedMoves), nl,
    write('Generated Isolated Moves: '), write(IsolatedMoves), nl.


non_isolated_moves(FromRow, FromCol, ToRow, ToCol, StackPosition, [Board, CurrentPlayer, BoardSize]):-
    %check is not empty
    adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
    within_bounds(ToRow, ToCol, BoardSize),
    is_not_empty(Board, [ToRow, ToCol]),
    get_stack_pieces(Board, FromRow, FromCol, StackPieces),
    valid_merge(Board, StackPieces, [FromRow,FromCol,ToRow, ToCol], StackPosition, CurrentPlayer),
    StackPosition \= [].



valid_merge(Board,StackPieces, [FromRow, FromCol, ToRow, ToCol], StackPosition, CurrentPlayer) :-
    get_stack_pieces(Board, ToRow, ToCol, ToStack),
    length(ToStack, ToStackLength),
    % Generate all possible merges
    findall(Index,
            (nth1(Index, StackPieces, Piece),
             Piece == CurrentPlayer,
             length(StackPieces, StackPiecesLength),
             TotalLength is ToStackLength + StackPiecesLength,
             TotalLength =< 8,
             Index =< ToStackLength),
            StackPosition).    

isolated_moves(FromRow, FromCol, ToRow, ToCol, BoardSize) :-
    adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
    within_bounds(ToRow, ToCol, BoardSize).

get_stack_pieces(Board, Row, Col, StackPieces) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Stack),
    Stack \= [],
    StackPieces = Stack.


piece_specific_moves(Moves, ListSpecificMoves, FromRow, FromCol) :-
    findall([FromRow, FromCol, ToRow, ToCol],
            (member([FromRow, FromCol, ToRow, ToCol], Moves),
             FromRow == FromRow, FromCol == FromCol),
            ListSpecificMoves).

    

is_not_empty(Board, [ToRow, ToCol]) :-
    nth1(ToRow, Board, BoardRow),
    nth1(ToCol, BoardRow, Stack),
    Stack \= [].



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