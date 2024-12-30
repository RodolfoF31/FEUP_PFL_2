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
    format('Player ~w, select the piece to move:~n', [Char]),
    repeat,
    read_position(FromRow, FromCol, BoardSize),
    get_piece(Board, FromRow, FromCol, Piece, CurrentPlayer),
    write('--- Piece Selected ---'), nl,
    get_player_move(GameState, NewGameState).
    %piece_belongs_to_player(Piece, CurrentPlayer), !.
    %format('Player ~w, enter the position to move the piece:~n', [CurrentPlayer]),
    %read_position(ToRow, ToCol, BoardSize),
    %write('--- Move ---'), write(FromCol), write(FromRow), write(' to '), write(ToCol), write(ToRow), nl,
    %move_piece(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    %NewGameState = [NewBoard, CurrentPlayer, BoardSize].

% Helper predicate to get the piece at a specific position on the board
get_piece(Board, Row, Col, Piece,CurrentPlayer) :-
   
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece),
    write('| Piece: '), write(Piece), nl,
    (Piece = [] -> 
        (write('Invalid move: No piece at the selected position.'), nl, fail); 
        (Piece = [CurrentPlayer] -> 
            (write('Valid move: The piece belongs to you.'), nl, true); 
            (write('Invalid move: The piece does not belong to you.'), nl, fail))).

% Helper predicate to check if a piece belongs to the current player
piece_belongs_to_player(Piece, Player) :-
    % Define the logic to check if the piece belongs to the player
    % This is a placeholder and should be replaced with actual game logic
    Piece = Player.

% Helper predicate to move a piece on the board
move_piece(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    % Define the logic to move the piece on the board
    % This is a placeholder and should be replaced with actual game logic
    NewBoard = Board.