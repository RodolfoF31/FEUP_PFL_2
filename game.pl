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
    GameState = [Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],
    
    display_game(GameState),

    % IF 0 -> GET PLAYER_MOVE, IF 1 -> GET COMPUTER MOVE lvl 1, IF 2 -> GET COMPUTER MOVE lvl 2
    ( game_over(GameState, Winner) ->
        format('Game over! The winner is ~w.', [Winner]), nl
    ;
        get_player_move(GameState, TempState),
        TempState = [TempBoard, NextPlayer, BoardSize, TempPlayer1Points, TempPlayer2Points, Player1Type, Player2Type],
        check_stack_of_8([TempBoard, NextPlayer, BoardSize, TempPlayer1Points, TempPlayer2Points, Player1Type, Player2Type], NewGameState),
        game_loop(NewGameState)
    ).

game_loop_pvc(GameState) :-
    GameState = [Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],

    display_game(GameState),

    (game_over(GameState, Winner) ->
        format('Game over! The winner is ~w.', [Winner]), nl
    ;
        (CurrentPlayer =:= 1 ->
            get_player_move(GameState, TempState)
        ;
            
            play_computer_move(GameState, Player2Type, TempState)
        ),
        TempState = [TempBoard, NextPlayer, BoardSize, TempPlayer1Points, TempPlayer2Points, Player1Type, Player2Type],
        check_stack_of_8([TempBoard, NextPlayer, BoardSize, TempPlayer1Points, TempPlayer2Points, Player1Type, Player2Type], NewGameState),
        game_loop_pvc(NewGameState)
    ).

game_loop_cvc(GameState) :-
    GameState = [Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],

    display_game(GameState),

    (game_over(GameState, Winner) ->
        format('Game over! The winner is ~w.', [Winner]), nl
    ;

        (CurrentPlayer =:= 1 ->
            play_computer_move(GameState, Player1Type, TempState)
        ;
            play_computer_move(GameState, Player2Type, TempState)
        ),
        TempState = [TempBoard, NextPlayer, BoardSize, TempPlayer1Points, TempPlayer2Points, Player1Type, Player2Type],
        check_stack_of_8([TempBoard, NextPlayer, BoardSize, TempPlayer1Points, TempPlayer2Points, Player1Type, Player2Type], NewGameState),
        sleep(1),
        game_loop_cvc(NewGameState)
    ).
