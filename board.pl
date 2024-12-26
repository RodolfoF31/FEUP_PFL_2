player_char(0,32). % Empty tale, space
player_char(1,79). % player1 is White
player_char(-1,88). % player2 is Black

row('A').
row('B').
row('C').
row('D').
row('E').
row('F').
row('G').
row('H').

col(1).
col(2).
col(3).
col(4).
col(5).
col(6).
col(7).
col(8).


% pieces of the board
color('White').
color('Black').

%stack has (number, topPiece)
%stack(Number, TopPiece).

% piece has (color,row,pos)
piece('White',row('B'),col(2)).
piece('White',row('B'),col(4)).
piece('White',row('B'),col(6)).
piece('White',row('B'),col(8)).
piece('White',row('D'),col(2)).
piece('White',row('D'),col(4)).
piece('White',row('D'),col(6)).
piece('White',row('D'),col(8)).
piece('White',row('F'),col(2)).
piece('White',row('F'),col(4)).
piece('White',row('F'),col(6)).
piece('White',row('F'),col(8)).

piece('Black',row('C'),col(1)).
piece('Black',row('C'),col(3)).
piece('Black',row('C'),col(5)).
piece('Black',row('C'),col(7)).
piece('Black',row('E'),col(1)).
piece('Black',row('E'),col(3)).
piece('Black',row('E'),col(5)).
piece('Black',row('E'),col(7)).
piece('Black',row('G'),col(1)).
piece('Black',row('G'),col(3)).
piece('Black',row('G'),col(5)).
piece('Black',row('G'),col(7)).

% predicate that clears the screen
clear_screen :- write('\33\[2J'), !.