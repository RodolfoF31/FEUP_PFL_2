option_size(1, 8).
option_size(2, 10).


%%%% Controller %%%%
menu :-
    byte_logo, 
    % Display the full menu
    menu_header_format('BYTE GAME'),
    menu_empty_format,
    menu_second_header_format('Option' , 'Details'),
    menu_empty_format,
    menu_option_format(1, 'Player vs Player'),
    menu_empty_format,
    menu_option_format(0, 'EXIT'),
    menu_empty_format,
    menu_end_format,

    % Read the user input && selects mode
    read_number(4, Number),
    menu_option(Number).

menu_choose_size(BoardSize):-
    menu_header_format('Choose a size to the board'),
    menu_empty_format,
    menu_second_header_format('Option', 'Details'),
    menu_option_format(1, '8x8'),
    menu_option_format(2, '10x10'),
    menu_empty_format,
    menu_option_format(0,'EXIT'),
    menu_end_format,
    read_number(2,Size),
    option_size(Size, BoardSize),
    write('Size chosen: '), write(BoardSize), nl.

menu_option(1) :-

    write('Starting Player vs Player mode...'), nl,
    
    %hardcode but should ask user is wants 8x8 or 10x10
    menu_choose_size(BoardSize),
    initial_state([_,BoardSize],[_,1]).




% INPUT OPERATIONS 
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






%%%% VIEW %%%%
% draws the byte logo
byte_logo :-
    write('               ####   ##   ##  ########   ########'), nl,
    write('               #   #   ## ##      ##      ###'), nl,
    write('               #   #     #        ##      ###'), nl,
    write('               ####      #        ##      #######'), nl,
    write('               #   #     #        ##      ###'), nl,
    write('               #   #     #        ##      ###'), nl,
    write('               ####      #        ##      ########'), nl,
    write('                                                  '), nl,
    write('                                                  ').

menu_header_format(Header):-
  format( ' ~n~`*t ~p ~`*t~57|~n', [Header]).


menu_empty_format :-
  format( '*~t*~57|~n', []).


menu_option_format(Option, Details):-
  format( '*~t~d~t~15|~t~a~t~40+~t*~57|~n', [Option, Details]).


menu_second_header_format(Label1, Label2):-
      format( '*~t~a~t~15+~t~a~t~40+~t*~57|~n', [Label1, Label2]).

menu_end_format :-
  format( '~`*t~57|~n', []).
