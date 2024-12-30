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

% move_stack(+GameState, +Player, +From, +To, -NewGameState)

move_stack(GameState, Player, From, To, NewGameState) :-
    GameState = [Board | Rest],       % Decompose the game state
    valid_dark_square(From),          % Ensure From is a dark square
    is_stack_owned(Board, Player, From), % Check that the stack belongs to Player
    is_stack_isolated(Board, From),      % Ensure the stack is isolated
    closest_stack(Board, From, Closest), % Find the closest stack
    valid_closer_move(From, Closest, To), % Find a valid move closer to the nearest stack
    perform_stack_move(Board, From, To, NewBoard), % Perform the move
    NewGameState = [NewBoard | Rest]. % Update the game state

% Helper predicate to find the closest stack to the given stack.
closest_stack(Board, Row-Col, ClosestRow-ClosestCol) :-
    findall(R-C, has_stack(Board, R-C), StackPositions), % Get all stack positions
    exclude(=(Row-Col), StackPositions, OtherStacks),    % Exclude the current stack
    maplist(stack_distance(Row-Col), OtherStacks, Distances), % Calculate distances
    min_member(MinDistance-ClosestRow-ClosestCol, Distances). % Find the closest stack

% Calculate the distance between two stacks.
stack_distance(Row1-Col1, Row2-Col2, Distance-Row2-Col2) :-
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).


% Check if a move brings the stack closer to the target.
valid_closer_move(FromRow-FromCol, ClosestRow-ClosestCol, ToRow-ToCol) :-
    adjacent(FromRow-FromCol, ToRow-ToCol), % Ensure the move is adjacent
    stack_distance(ToRow-ToCol, ClosestRow-ClosestCol, NewDistance),
    stack_distance(FromRow-FromCol, ClosestRow-ClosestCol, CurrentDistance),
    NewDistance < CurrentDistance. % Ensure the move reduces the distance

% Helper predicate to check if a square is dark (sum of coordinates is odd).
valid_dark_square(Row-Col) :-
    Sum is Row + Col,
    Sum mod 2 =:= 1.

% Check if a stack is owned by the player (bottom checker matches Player).
is_stack_owned(Board, Player, Row-Col) :-
    nth1(Row, Board, RowList), % Get the row
    nth1(Col, RowList, Stack), % Get the stack at the column
    Stack = [Bottom | _],      % Extract the bottom checker
    Bottom =:= Player.         % Ensure it matches the Player

% Check if a stack is isolated (no adjacent stacks exist).
is_stack_isolated(Board, Row-Col) :-
    findall(RowAdj-ColAdj, adjacent(Row-Col, RowAdj-ColAdj), AdjacentSquares),
    \+ (member(Square, AdjacentSquares), has_stack(Board, Square)).

% Helper predicate to find adjacent squares.
adjacent(Row-Col, RowAdj-ColAdj) :-
    member(DeltaRow-DeltaCol, [-1-1, -1-1, 1-1, 1-1]), % Diagonal directions
    RowAdj is Row + DeltaRow,
    ColAdj is Col + DeltaCol,
    RowAdj > 0, ColAdj > 0, RowAdj =< 8, ColAdj =< 8. % Stay within the board bounds

% Check if a square has a stack.
has_stack(Board, Row-Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Stack),
    Stack \= [].

% Check if the move is valid (diagonal and adjacent).
valid_diagonal_move(RowFrom-ColFrom, RowTo-ColTo) :-
    abs(RowFrom - RowTo) =:= 1, % Move is one square diagonally
    abs(ColFrom - ColTo) =:= 1.

% Perform the stack move by updating the board.
perform_stack_move(Board, RowFrom-ColFrom, RowTo-ColTo, NewBoard) :-
    nth1(RowFrom, Board, RowListFrom),
    nth1(ColFrom, RowListFrom, StackFrom),
    replace(Board, RowFrom-ColFrom, [], TempBoard), % Clear the original square
    replace(TempBoard, RowTo-ColTo, StackFrom, NewBoard).

% Replace the contents of a specific square on the board.
replace(Board, Row-Col, NewContent, NewBoard) :-
    nth1(Row, Board, RowList, RestRows), % Extract the target row
    nth1(Col, RowList, _, RestCols),    % Extract the target column
    nth1(Col, NewRow, NewContent, RestCols), % Replace column content
    nth1(Row, NewBoard, NewRow, RestRows). % Replace row in the board

% merge_stacks(+GameState, +Player, +From, +To, -NewGameState)
% Merges a stack at position From with a stack at position To for Player if valid.
% GameState: The current board state.
% Player: The player attempting the merge (1 or 2).
% From: The coordinates of the source stack (RowFrom-ColFrom).
% To: The coordinates of the target stack (RowTo-ColTo).
% NewGameState: The updated game state after the merge.

merge_stacks(GameState, Player, From, To, NewGameState) :-
    GameState = [Board | Rest],                % Decompose the game state
    valid_dark_square(From),                  % Ensure From is a dark square
    valid_dark_square(To),                    % Ensure To is a dark square
    is_stack_owned(Board, Player, From),      % Check that the source stack belongs to Player
    adjacent(From, To),                       % Ensure the stacks are adjacent
    stack_height(Board, From, FromHeight),    % Get the height of the source stack
    stack_height(Board, To, ToHeight),        % Get the height of the target stack
    FromHeight > 0,                           % Source stack must not be empty
    ToHeight < 8,                             % Target stack must have fewer than 8 checkers
    FromHeight + ToHeight =< 8,               % Ensure the resulting stack does not exceed 8
    FromHeight > ToHeight,                    % Ensure the checker is moved to a higher altitude
    perform_merge(Board, From, To, NewBoard), % Perform the merge
    NewGameState = [NewBoard | Rest].         % Update the game state

% Helper predicate to check adjacency of two positions.
adjacent(Row1-Col1, Row2-Col2) :-
    DiffRow is abs(Row1 - Row2),
    DiffCol is abs(Col1 - Col2),
    DiffRow =:= 1, DiffCol =:= 1. % Adjacent squares are diagonal only

% Helper predicate to perform the merge by updating the board.
perform_merge(Board, RowFrom-ColFrom, RowTo-ColTo, NewBoard) :-
    nth1(RowFrom, Board, RowListFrom),
    nth1(ColFrom, RowListFrom, StackFrom),         % Get the source stack
    nth1(RowTo, Board, RowListTo),
    nth1(ColTo, RowListTo, StackTo),               % Get the target stack
    append(StackFrom, StackTo, NewStack),          % Merge stacks with source on top
    replace(Board, RowFrom-ColFrom, [], TempBoard), % Clear the source square
    replace(TempBoard, RowTo-ColTo, NewStack, NewBoard).

% Get the height of a stack at a specific position.
stack_height(Board, Row-Col, Height) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Stack),
    length(Stack, Height).

% move(+GameState, +Move, -NewGameState)
% Validates and executes a move, updating the game state if valid.
move(GameState, basic_move(Player, From, To), NewGameState) :-
    execute_move(GameState, basic_move(Player, From, To), NewGameState).

move(GameState, merge(Player, From, To), NewGameState) :-
    execute_move(GameState, merge(Player, From, To), NewGameState).

% Helper predicate to execute a move and update the game state
execute_move(GameState, Move, NewGameState) :-
    GameState = [Board | Rest],
    ( Move = basic_move(Player, From, To) ->
        basic_move(GameState, Player, From, To, UpdatedBoard)
    ; Move = merge(Player, From, To) ->
        merge_stacks(GameState, Player, From, To, UpdatedBoard)
    ),
    NewGameState = [UpdatedBoard | Rest].

% valid_moves(+GameState, -ListOfMoves)
% Generates a list of all valid moves (basic moves and merges) for the current player.
valid_moves(GameState, ListOfMoves) :-
    GameState = [Board, Player | _],
    findall(basic_move(Player, From, To),
            (member(Row-Column, all_positions),       % Generate all valid positions
             valid_dark_square(Row-Column),
             is_stack_owned(Board, Player, Row-Column),
             basic_move(GameState, Player, Row-Column, To, _)), % Test basic move validity
            BasicMoves),
    findall(merge(Player, From, To),
            (member(Row-Column, all_positions),      % Generate all valid positions
             valid_dark_square(Row-Column),
             is_stack_owned(Board, Player, Row-Column),
             adjacent(Row-Column, To),
             merge_stacks(GameState, Player, Row-Column, To, _)), % Test merge move validity
            MergeMoves),
    append(BasicMoves, MergeMoves, ListOfMoves).

% Helper predicate to generate all board positions.
all_positions(List) :-
    findall(Row-Column, (between(1, 8, Row), between(1, 8, Column)), List).
