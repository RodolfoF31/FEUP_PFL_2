% Make it so that the board size can be dymanic

%%%% CONTROLLER %%%%


initial_state(GameConfig, GameState):-

    % Create the board and player according to the game configuration (mode and boards size)
    GameConfig = [Mode, BoardSize],

    
    % Initialize the game state
    GameState = [Board, CurrentPlayer],
    initialize_board(BoardSize, Board),
    assert(curr_board(Board)),



player_char(0, '').  % Empty tile as a dot
player_char(1, 'O').  % Player1 is White ('O')
player_char(-1, 'X'). % Player2 is Black ('X')

row(1, 'A').
row(2, 'B').
row(3, 'C').
row(4, 'D').
row(5, 'E').
row(6, 'F').
row(7, 'G').
row(8, 'H').
row(9, 'I').
row(10, 'J').



col(1, '1').
col(2, '2').
col(3, '3').
col(4, '4').
col(5, '5').
col(6, '6').
col(7, '7').
col(8, '8').
col(9, '9').
col(10, '10').

% Initialize an 8x8 empty board
empty_board_8x8([
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []]
]).

empty_board_10x10([
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], [], []]
]).

% Set initial pieces on the board
initialize_board(BoardSize, Board) :-
    (BoardSize == 8 -> empty_board_8x8(Board);
     BoardSize == 10 -> empty_board_10x10(Board)).

set_pieces(Board, FinalBoard) :-
    set_white_pieces(Board, TempBoard),
    set_black_pieces(TempBoard, FinalBoard).

% Place a piece on the board
set_piece(Board, Row, Col, Player, UpdatedBoard) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    append(Cell, Player, NewCell),
    set_row(BoardRow, Col, NewCell, NewRow),
    set_row(Board, Row, NewRow, UpdatedBoard).



set_white_pieces(Board, UpdatedBoard) :-
    set_piece(Board, 7, 1, [1], Temp1),
    set_piece(Temp1, 7, 3, [1], Temp2),
    set_piece(Temp2, 7, 5, [1], Temp3),
    set_piece(Temp3, 7, 7, [1], Temp4),
    set_piece(Temp4, 5, 1, [1], Temp5),
    set_piece(Temp5, 5, 3, [1], Temp6),
    set_piece(Temp6, 5, 5, [1], Temp7),
    set_piece(Temp7, 5, 7, [1], Temp8),
    set_piece(Temp8, 3, 1, [1], Temp9),
    set_piece(Temp9, 3, 3, [1], Temp10),
    set_piece(Temp10, 3, 5, [1], Temp11),
    set_piece(Temp11, 3, 7, [1], UpdatedBoard).

% Place Black pieces at the specified positions
set_black_pieces(Board, UpdatedBoard) :-
    set_piece(Board, 6, 2, [-1], Temp1),
    set_piece(Temp1, 6, 4, [-1], Temp2),
    set_piece(Temp2, 6, 6, [-1], Temp3),
    set_piece(Temp3, 6, 8, [-1], Temp4),
    set_piece(Temp4, 4, 2, [-1], Temp5),
    set_piece(Temp5, 4, 4, [-1], Temp6),
    set_piece(Temp6, 4, 6, [-1], Temp7),
    set_piece(Temp7, 4, 8, [-1], Temp8),
    set_piece(Temp8, 2, 2, [-1], Temp9),
    set_piece(Temp9, 2, 4, [-1], Temp10),
    set_piece(Temp10, 2, 6, [-1], Temp11),
    set_piece(Temp11, 2, 8, [-1], UpdatedBoard).

% Replace element in a row
set_row([_|T], 1, NewElem, [NewElem|T]).
set_row([H|T], Index, NewElem, [H|R]) :-
    Index > 1,
    Index1 is Index - 1,
    set_row(T, Index1, NewElem, R).


next_col(CurrentCol, NextCol) :-
    char_code(CurrentCol, Code),
    NextCode is Code + 1,
    char_code(NextCol, NextCode).










%%%% VIEW %%%%

% Display the game state
display_game(GameState) :-
    GameState = [Board, CurrentPlayer],
    write('Current Player: '),
    (CurrentPlayer = 1 -> write('White (O)'); write('Black (X)')), nl, nl,
    display_board(Board).


% Display the board
display_board(Board) :-
    clear_screen,
    write('    A   B   C   D   E   F   G   H'), nl,
    write('  +---+---+---+---+---+---+---+---+'), nl,
    display_rows(Board, 8), nl,
    display_stacks_info(Board, 8, 'A').

display_rows([], _).
display_rows([Row|Rest], RowLabel) :-
    write(RowLabel), write(' |'),
    display_cells(Row),
    write('|'),  
    nl,
    write('  +---+---+---+---+---+---+---+---+'), nl,
    NextRowLabel is RowLabel - 1,
    display_rows(Rest, NextRowLabel).

display_cells([]).
display_cells([Cell|Rest]) :-
    display_cell(Cell),
    write('|'),  % Add a | between cells
    display_cells(Rest).

% Display an empty cell
display_cell([]) :-
    write('   '). % Empty cells display as spaces.

% Display a cell with a list (show the last element in the list)
display_cell(Cell) :-
    Cell \= [],
    last(Cell, Player),          % Extract the last element of the list
    player_char(Player, Char),   % Get the player's display character
    write(' '), write(Char), write(' ').

clear_screen :- write('\e[2J'), !.

% Display stacks information
display_stacks_info([], _, _).
display_stacks_info([Row|Rest], RowLabel, ColLabel) :-
    display_row_stacks_info(Row, RowLabel, ColLabel, 'A'),
    NextRowLabel is RowLabel - 1,
    display_stacks_info(Rest, NextRowLabel, ColLabel).

display_row_stacks_info([], _, _, _).
display_row_stacks_info([Cell|Rest], RowLabel, ColLabel, CurrentCol) :-
    (Cell \= [] ->
        length(Cell, Size),
        maplist(player_char, Cell, CharList), % Map numeric values to characters
        format('Stack of Row ~w and Column ~w: ~w, Size ~d~n', [RowLabel, CurrentCol, CharList, Size])
    ; true),
    next_col(CurrentCol, NextCol),
    display_row_stacks_info(Rest, RowLabel, ColLabel, NextCol).
