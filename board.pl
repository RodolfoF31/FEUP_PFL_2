player_char(0,32). % Empty tale, space
player_char(1,79). % player1 is White
player_char(-1,88). % player2 is Black

row(1,'A').
row(2,'B').
row(3,'C').
row(4,'D').
row(5,'E').
row(6,'F').
row(7,'G').
row(8,'H').

% predicate that clears the screen
clear_screen :- write('\33\[2J'), !.