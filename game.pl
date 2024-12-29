:-consult('menus.pl').
:-consult('inputs.pl').
:-consult('board.pl').
:-consult('logic.pl').

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).



% Main function

play :-
    clear_screen,
    menu.

game_loop(GameState) :-
    repeat,
        initialize_game,
        %input_loop,
        %display_gameState(GameState),
    !.
    
