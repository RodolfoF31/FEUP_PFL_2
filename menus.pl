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




menu :-
    byte_logo, 

    menu_header_format('BYTE GAME'),
    menu_empty_format,
    menu_second_header_format('Option' , 'Details'),
    menu_empty_format,
    menu_option_format(1, 'Player vs Player'),
    menu_empty_format,
    menu_option_format(0, 'EXIT'),
    menu_empty_format,
    menu_end_format,

    read_number(4, Number),
    menu_option(Number).