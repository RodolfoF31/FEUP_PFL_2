% Predicate to read a valid row (1-8 or 1-10 depending on the board size)
read_row(RowIndex, BoardSize) :-
    repeat,
    format('| Enter a Row (1-~d): ', [BoardSize]),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),
    number_char(Char, RowIndex),  % Convert to number
    RowIndex >= 1,
    RowIndex =< BoardSize,   % Ensure the row is within the board size
    skip_line, !.

% Map valid row characters (A-H) to their corresponding indices
valid_row(Char, Index) :-
    char_code('A', ACode),
    char_code(Char, Code),
    Code >= ACode,
    Code =< ACode + 7,         % 'A' to 'H'
    Index is Code - ACode + 1. % Convert to index (1-8)

% Predicate to read a valid column (A-H or A-J depending on the board size)
read_column(ColumnIndex, BoardSize) :-
    repeat,
    (BoardSize == 8 -> MaxCol = 'H'; MaxCol = 'J'),
    format('| Enter a Column (A-~w): ', [MaxCol]),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),
    valid_column(Char, ColumnIndex, BoardSize),   % Check if the column is valid and get its index
    skip_line, !.

% Map valid column characters (A-H or A-J) to their corresponding indices
valid_column(Char, Index, BoardSize) :-
    char_code('A', ACode),
    char_code(Char, Code),
    Code >= ACode,
    (BoardSize == 8 -> MaxCode is ACode + 7; MaxCode is ACode + 9),  % 'A' to 'H' or 'A' to 'J'
    Code =< MaxCode,
    Index is Code - ACode + 1.  % Convert to index (1-8 or 1-10)

% Predicate to read a valid position on the board (Row, Column)
read_position(RowIndex, ColumnIndex, BoardSize) :-
    write('--- Input Position ---'), nl,
    read_row(RowIndex, BoardSize),
    read_column(ColumnIndex, BoardSize),
    format('| Selected Position: Row ~d, Column ~d', [RowIndex, ColumnIndex]), nl.

%play PvP
get_player_move(GameState, NewGameState) :-
    GameState = [Board, CurrentPlayer, BoardSize],
    player_char(CurrentPlayer, Char),

    %call valid_moves
    %ask what piece ?
    %FILTER PIECE FORM MOVES LIST "[2,2,1,1],[2,2,1,3],[2,2,3,1],[2,2,3,3]"

    %validate piece
    %DISPLAY VALID MOVES
    %SELECT MOVE

    format('Player ~w, select the piece to move:~n', [Char]),
    repeat,
    %read_position(FromRow, FromCol, BoardSize),
    %get_piece(Board, FromRow, FromCol, Piece, CurrentPlayer),
    %write('--- Piece Selected ---'), nl,
    write('------------------------------------------------------------'), nl,
    valid_moves(GameState, Moves),
    display_moves(Moves),
    read_position(FromRow, FromCol, BoardSize),

    NextPlayer is -CurrentPlayer,
    NewGameState = [Board, NextPlayer, BoardSize].
    %piece_belongs_to_player(Piece, CurrentPlayer), !.
    %format('Player ~w, enter the position to move the piece:~n', [CurrentPlayer]),
    %read_position(ToRow, ToCol, BoardSize),
    %write('--- Move ---'), write(FromCol), write(FromRow), write(' to '), write(ToCol), write(ToRow), nl,
    %move_piece(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    %NewGameState = [NewBoard, CurrentPlayer, BoardSize].



display_moves(Moves) :-
    write('--- Valid Moves ---'), nl,
    display_moves(Moves, 1).

display_moves([], _).
display_moves([[FromRow, FromCol, ToRow, ToCol] | Rest], Index) :-
    format('~d: Move from (~d, ~d) to (~d, ~d)~n', [Index, FromRow, FromCol, ToRow, ToCol]),
    NextIndex is Index + 1,
    display_moves(Rest, NextIndex).

