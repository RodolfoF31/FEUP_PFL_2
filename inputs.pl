% Predicate to read a number between a given range
read_number(UpperBound, Result) :-
    repeat,
    format('| Choose an Option (0-~d) - ', [UpperBound]),
    get_code(ASCIICode),
    peek_char(Enter),
    Enter == '\n',
    char_code(Char, ASCIICode),  % Convert ASCII code to character
    number_char(Char, Result),   % Convert the character to a number
    skip_line,
    UP is UpperBound + 1,
    % Check if the number is within the bounds (0 to UpperBound)
    Result >= 0, Result < UP, !.

% Convert a character to a number
number_char(Char, Result) :-
    char_code(Char, Code),
    Result is Code - 48.   % Subtract 48 to convert ASCII of '0'-'9' to the actual number

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

