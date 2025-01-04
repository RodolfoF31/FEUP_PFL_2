:-consult('menus.pl').
:-consult('inputs.pl').
:-consult('board.pl').
:-consult('logic.pl').

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).




% Main function
% 

play :-
    clear_screen,
    menu.
%[Board, CurrentPlayer, BoardSize,Mode,POintsWHite,POintsBlack]
% Mode = PVP ->         NextPlayer is -CurrentPlayer, NewGameState = [NewBoard, NextPlayer, BoardSize],
% Mode = PVC ->         NextPlayer is CUSTOMVAR, NewGameState = [NewBoard, NextPlayer, BoardSize],
% MODE = CVC ->         NEXTPLAYER IS CUSTOMVAR2, NEWGAMESTATE = [NEWBOARD, NEXTPLAYER, BOARDSIZE],     
game_loop(GameState) :-
    GameState = [Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],
    
    check_stack_of_8([Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type], [NewBoard, CurrentPlayer, BoardSize, NewPlayer1Points, NewPlayer2Points, Player1Type, Player2Type]),
    display_game(GameState),

    % IF 0 -> GET PLAYER_MOVE, IF 1 -> GET COMPUTER MOVE lvl 1, IF 2 -> GET COMPUTER MOVE lvl 2
    ( game_over([NewBoard, CurrentPlayer, BoardSize, NewPlayer1Points, NewPlayer2Points, Player1Type, Player2Type], Winner) ->
        format('Game over! The winner is ~w.', [Winner]), nl
    ;
        get_player_move([NewBoard, CurrentPlayer, BoardSize, NewPlayer1Points, NewPlayer2Points, Player1Type, Player2Type], NewGameState),
        game_loop(NewGameState)
    ).

    
    
