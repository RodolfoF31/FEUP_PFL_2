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

col(1, '1').
col(2, '2').
col(3, '3').
col(4, '4').
col(5, '5').
col(6, '6').
col(7, '7').
col(8, '8').

% Initialize an 8x8 empty board
empty_board([
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], []]
]).

% Set initial pieces on the board
initialize_board(Board) :-
    empty_board(EmptyBoard),
    set_pieces(EmptyBoard, Board).

set_pieces(Board, FinalBoard) :-
    set_white_pieces(Board, TempBoard),
    set_black_pieces(TempBoard, FinalBoard).

% Place a piece on the board
set_piece(Board, Row, Col, Player, UpdatedBoard) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    append(Cell, [Player], NewCell),
    set_row(BoardRow, Col, NewCell, NewRow),
    set_row(Board, Row, NewRow, UpdatedBoard).

% Place White pieces at the specified positions
set_white_pieces(Board, UpdatedBoard) :-
    set_piece(Board, 7, 1, 1, Temp1),
    set_piece(Temp1, 7, 3, 1, Temp2),
    set_piece(Temp2, 7, 5, 1, Temp3),
    set_piece(Temp3, 7, 7, 1, Temp4),
    set_piece(Temp4, 5, 1, 1, Temp5),
    set_piece(Temp5, 5, 3, 1, Temp6),
    set_piece(Temp6, 5, 5, 1, Temp7),
    set_piece(Temp7, 5, 7, 1, Temp8),
    set_piece(Temp8, 3, 1, 1, Temp9),
    set_piece(Temp9, 3, 3, 1, Temp10),
    set_piece(Temp10, 3, 5, 1, Temp11),
    set_piece(Temp11, 3, 7, 1, UpdatedBoard).

% Place Black pieces at the specified positions
set_black_pieces(Board, UpdatedBoard) :-
    set_piece(Board, 6, 2, -1, Temp1),
    set_piece(Temp1, 6, 4, -1, Temp2),
    set_piece(Temp2, 6, 6, -1, Temp3),
    set_piece(Temp3, 6, 8, -1, Temp4),
    set_piece(Temp4, 4, 2, -1, Temp5),
    set_piece(Temp5, 4, 4, -1, Temp6),
    set_piece(Temp6, 4, 6, -1, Temp7),
    set_piece(Temp7, 4, 8, -1, Temp8),
    set_piece(Temp8, 2, 2, -1, Temp9),
    set_piece(Temp9, 2, 4, -1, Temp10),
    set_piece(Temp10, 2, 6, -1, Temp11),
    set_piece(Temp11, 2, 8, -1, UpdatedBoard).

% Replace element in a row
set_row([_|T], 1, NewElem, [NewElem|T]).
set_row([H|T], Index, NewElem, [H|R]) :-
    Index > 1,
    Index1 is Index - 1,
    set_row(T, Index1, NewElem, R).

% Display the board
display_board(Board) :-
    clear_screen,
    write('    A   B   C   D   E   F   G   H'), nl,
    write('  +---+---+---+---+---+---+---+---+'), nl,
    display_rows(Board, 8).

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

display_cell([]) :- write('   ').
display_cell([Player|_]) :-
    player_char(Player, Char),
    write(' '), write(Char), write(' ').

clear_screen :- write('\e[2J'), !.


% Display the game state
display_game(GameState) :-
    GameState = [Board, CurrentPlayer],
    write('Current Player: '),
    (CurrentPlayer = 1 -> write('White (O)'); write('Black (X)')), nl, nl,
    display_board(Board).