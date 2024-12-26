% Predicate to read a number between a given range
read_number(UpperBound, Result) :-
    repeat,
    format('| Choose an Option (~d-~d) - ', [0, UpperBound]),
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
