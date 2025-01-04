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

%Player 1 -> O || Player 2 -> X
game_loop(GameState) :-
    repeat,
        display_game(GameState),
        get_player_move(GameState, NewGameState),
        game_loop(NewGameState).
