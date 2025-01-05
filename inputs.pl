read_optiontoplay(Number, Result) :-
    repeat,
    get_code(ASCIICode),
    char_code(Char, ASCIICode),
    number_char(Char, Result),
    Result >= 0,
    Result =< Number,
    skip_line, % Consume the rest of the line
    !.


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

read_index(Index, BoardSize) :-
    repeat,  
    write('| Enter an Index: '),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),  % Convert ASCII code to character
    number_char(Char, Index),    % Convert the character to a number
    Index >= 1, Index =< BoardSize,  % Validate the range
    skip_line, 
    !.  




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
    format('| Enter a Column (1-~d): ', [BoardSize]),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),
    number_char(Char, ColumnIndex),  % Convert to number
    ColumnIndex >= 1,
    ColumnIndex =< BoardSize,   % Ensure the column is within the board size
    skip_line, !.

stack_belongs_to_player(Board, Row, Col, CurrentPlayer) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Stack),
    Stack \= [],
    last(Stack, BottomPiece),
    BottomPiece = CurrentPlayer.


% Predicate to read a valid position on the board (Row, Column)
read_position(RowIndex, ColumnIndex, BoardSize) :-
    write('--- Input Position ---'), nl,
    read_row(RowIndex, BoardSize),
    read_column(ColumnIndex, BoardSize),
    column_letter(ColumnIndex, ColLetter),
    format('| Selected Position: Row ~d, Column ~w', [RowIndex, ColLetter]), nl.

is_valid_move(Moves, FromRow, FromCol, ToRow, ToCol, Index) :-
    member([FromRow, FromCol, ToRow, ToCol, Index], Moves).

has_valid_move(Moves, FromRow, FromCol) :-
    member([FromRow, FromCol, _, _, _], Moves).

get_player_action([Board, CurrentPlayer, BoardSize, _, _,_,_], FromRow, FromCol, NewGameState, Moves) :-
    write('--- Player Action ------------------------------------------------------------------------------------------------'), nl,
    ( \+ is_stack_isolated(Board, BoardSize, FromRow, FromCol) -> 
        write('The stack is not isolated. You must perform a merge.'), nl,
        write('Input to what stack you would like to merge and the index of the piece to split the stack'), nl,
        read_position(ToRow, ToCol, BoardSize),
        read_index(Index, BoardSize),
        ( is_valid_move(Moves, FromRow, FromCol, ToRow, ToCol, ValidIndexes), member(Index, ValidIndexes) ->
            move([Board, CurrentPlayer, BoardSize,_,_,_,_], [FromRow, FromCol, ToRow, ToCol, Index], NewGameState)
        ;
            write('Invalid move. Please try again.'), nl,
            get_player_action([Board, CurrentPlayer, BoardSize], FromRow, FromCol, NewGameState, Moves)
        ),
        !
    ;
      is_stack_isolated(Board, BoardSize, FromRow, FromCol) ->
        write('The stack is isolated. You can perform a basic move.'), nl,
        write('Input the position to move the stack'), nl,
        read_position(ToRow, ToCol, BoardSize),
        ( is_valid_move(Moves, FromRow, FromCol, ToRow, ToCol, 0) ->
            move([Board, CurrentPlayer, BoardSize,_,_,_,_], [FromRow, FromCol, ToRow, ToCol, 0], NewGameState)
        ;
            write('Invalid move! Please try again.'), nl,
            get_player_action([Board, CurrentPlayer, BoardSize], FromRow, FromCol, NewGameState, Moves)
        ),
        !
    ).




%play PvP
get_player_move(GameState, NewGameState) :-
    write('--- Player Move ------------------------------------------------------------------------------------------------'), nl,
    GameState = [Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],
    player_char(CurrentPlayer, Char),

    valid_moves(GameState, Moves),

    ( Moves = [] ->
        write('No valid moves available. Skipping turn...'), nl,
        NextPlayer is -CurrentPlayer,
        NewGameState = [Board, NextPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type]
        
    ;

    format('Player ~w, select the piece to move:~n', [Char]),
    repeat,
    read_position(FromRow, FromCol, BoardSize),
    ( has_valid_move(Moves, FromRow, FromCol) ->
        get_player_action([Board, CurrentPlayer, BoardSize, _, _, _, _], FromRow, FromCol, TempState, Moves),
        TempState = [NewBoard, CurrentPlayer, BoardSize, _ ,_,_,_],
        NextPlayer is -CurrentPlayer,
        NewGameState = [NewBoard, NextPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],
        write('------------------------------------------------------------'), nl,
        !
    ;
        write('You cannot play this stack right now.'), nl,
        fail
    )).



display_moves(Moves) :-
    write('--- Valid  ---'), nl,
    display_moves(Moves, 1).

display_moves([], _).
display_moves([[FromRow, FromCol, ToRow, ToCol] | Rest], Index) :-
    column_letter(FromCol, FromColLetter),
    column_letter(ToCol, ToColLetter),
    format('~d: Move from (~d, ~w) to (~d, ~w)~n', [Index, FromRow, FromColLetter, ToRow, ToColLetter]),
    NextIndex is Index + 1,
    display_moves(Rest, NextIndex).

column_letter(ColIndex, ColLetter) :-
    char_code('A', ACode),
    ColCode is ACode + ColIndex - 1,
    char_code(ColLetter, ColCode).

letter_column(ColLetter, ColIndex) :-
    char_code('A', ACode),
    char_code(ColLetter, ColCode),
    ColIndex is ColCode - ACode + 1.



play_computer_move(GameState, Level, NewGameState):-
    choose_move(GameState, Level, Move),
    (Move = [_, _, _, _, Indexes], is_list(Indexes) ->
        random_member(Index, Indexes),
        MoveWithIndex = [FromRow, FromCol, ToRow, ToCol, Index]
    ;
        MoveWithIndex = Move
    ),
    (Move \= [] -> move(GameState, Move, TempState);
    write('No valid moves available. Passing turn.'), nl
    ),
    TempState = [NewBoard, CurrentPlayer, BoardSize, _ ,_,_,_],
    NextPlayer is -CurrentPlayer,
    NewGameState = [NewBoard, NextPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type].

