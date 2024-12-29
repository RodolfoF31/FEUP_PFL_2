% Predicate to read a valid row (A-H) and convert it to the corresponding index (1-8)
read_row(RowIndex) :-
    repeat,
    write('| Enter a Row (A-H): '),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),
    valid_row(Char, RowIndex),   % Check if the row is valid and get its index
    skip_line, !.

% Map valid row characters (A-H) to their corresponding indices
valid_row(Char, Index) :-
    char_code('A', ACode),
    char_code(Char, Code),
    Code >= ACode,
    Code =< ACode + 7,         % 'A' to 'H'
    Index is Code - ACode + 1. % Convert to index (1-8)

% Predicate to read a valid column (1-8)
read_column(ColumnIndex) :-
    repeat,
    write('| Enter a Column (1-8): '),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),
    number_char(Char, ColumnIndex),  % Convert to number
    ColumnIndex >= 1,
    ColumnIndex =< 8,   % Ensure the column is between 1 and 8
    skip_line, !.

% Predicate to read a valid position on the board (Row, Column)
read_position(RowIndex, ColumnIndex) :-
    write('--- Input Position ---'), nl,
    read_row(RowIndex),
    read_column(ColumnIndex),
    format('| Selected Position: Row ~d, Column ~d', [RowIndex, ColumnIndex]), nl.

