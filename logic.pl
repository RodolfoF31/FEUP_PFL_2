:- use_module(library(lists)).

min_list([Min], Min).
min_list([H|T], Min) :-
    min_list(T, MinTail),
    Min is min(H, MinTail).

% Check if the game is over
game_over([_, _, _, Player1Points, Player2Points, _, _], Winner) :-
    ( Player1Points >= 2 ->
        Winner = 'Player 1'
    ; Player2Points >= 2 ->
        Winner = 'Player 2'
    ).

check_stack_of_8([Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type], [NewBoard, CurrentPlayer, BoardSize, NewPlayer1Points, NewPlayer2Points, Player1Type, Player2Type]) :-
    findall([Row, Col], (nth1(Row, Board, BoardRow), nth1(Col, BoardRow, Stack), length(Stack, 8)), StacksOf8),
    update_board_and_points(Board, StacksOf8, Player1Points, Player2Points, NewBoard, NewPlayer1Points, NewPlayer2Points).

update_board_and_points(Board, [], Player1Points, Player2Points, Board, Player1Points, Player2Points).
update_board_and_points(Board, [[Row, Col] | Rest], Player1Points, Player2Points, NewBoard, NewPlayer1Points, NewPlayer2Points) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Stack),
    last(Stack, TopPiece),
    ( TopPiece = 1 ->
        UpdatedPlayer1Points is Player1Points + 1,
        UpdatedPlayer2Points is Player2Points
    ; TopPiece = -1 ->
        UpdatedPlayer1Points is Player1Points,
        UpdatedPlayer2Points is Player2Points + 1
    ),
    set_piece_for_basicmove(Board, Row, Col, [], TempBoard),
    update_board_and_points(TempBoard, Rest, UpdatedPlayer1Points, UpdatedPlayer2Points, NewBoard, NewPlayer1Points, NewPlayer2Points).

% BASIC MOVES

get_piece(Board, Row, Col, Piece,CurrentPlayer) :-
   
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece),
    %format('Piece at (~d, ~d): ~w~n', [Row, Col, Piece]),
    (Piece = [] -> 
        ( fail); 
        (( Piece = [CurrentPlayer] ; Piece =[CurrentPlayer|_] ) -> 
            ( true); 
            (fail))).

player_piece(Board, Player, Row, Col) :-
    get_piece(Board, Row, Col, Piece, Player),
    Piece \= [].


% valid_moves(+GameState, -Moves)          GameState = [Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type],

valid_moves([Board, CurrentPlayer, BoardSize, _, _, _, _], Moves) :-
    findall([Row, Col], player_piece(Board, CurrentPlayer, Row, Col), Positions),
    findall([Row, Col], (nth1(Row, Board, BoardRow), nth1(Col, BoardRow, Piece), Piece \= []), AllPieces),
    % divide positions into isolated and non-isolated
    findall([Row, Col], (member([Row, Col], Positions), is_stack_isolated(Board, BoardSize, Row, Col)), IsolatedPositions),
    findall([Row, Col], (member([Row, Col], AllPieces), \+ is_stack_isolated(Board, BoardSize, Row, Col)), NonIsolatedPositions),

    % return the non-isolated
    % Find the correct moves for isolated positions and group by origin position
    (IsolatedPositions \= [] ->
        findall([FromRow, FromCol, ToRow, ToCol],
                (member([FromRow, FromCol], IsolatedPositions),
                 isolated_moves(FromRow, FromCol, ToRow, ToCol, BoardSize)),
                PossibleMoves),

        findall([FromRow,FromCol,Distance,ToRow,ToCol],
                (member(Move, PossibleMoves),
                Move = [FromRow, FromCol, ToRow, ToCol],
                 get_smallest_path(Move ,AllPieces, Distance, Results),
                 Results = [[_,_,Distance]|_]),
                PossibleMovesWithDistance),

        PossibleMovesWithDistance = [FirstMove | _],

        FilteredMovesResult = [],
        TempMovesFromPosition = [],
        filter_smalles_moves(PossibleMovesWithDistance, FirstMove, TempMovesFromPosition, FilteredMovesResult,FilteredMoves)
        
    ;
        FilteredMoves = []
    ),

    findall([FromRow, FromCol, ToRow, ToCol, StackPosition],
            (member([FromRow, FromCol], NonIsolatedPositions),
             non_isolated_moves(FromRow, FromCol, ToRow, ToCol, StackPosition, [Board, CurrentPlayer, BoardSize,_,_,_,_])),
            NonIsolatedMoves),

    append(NonIsolatedMoves, FilteredMoves, Moves),
    write('All Moves: '), write(Moves), nl.

change_last([_], NewValue, [NewValue]).
change_last([Head|Tail], NewValue, [Head|NewTail]) :-
    change_last(Tail, NewValue, NewTail). 

last_or_single([X], X).
last_or_single([_ | Tail], Last) :-
    last_or_single(Tail, Last).

filter_smalles_moves([], _, TempMovesFromPosition, FilteredMovesResult, FilteredMoves) :-
    last_or_single(TempMovesFromPosition, LastMove),
    change_last(TempMovesFromPosition, LastMove, TempMovesFromPosition1),
    append(TempMovesFromPosition1, FilteredMovesResult, FilteredMovesResult1),
    FilteredMoves = FilteredMovesResult1.
filter_smalles_moves([[FromRow, FromCol, Distance, ToRow, ToCol] | Rest], IterMove, TempMovesFromPosition, FilteredMovesResult, FilteredMoves) :-
    (IterMove = [IterFr, IterFc, IterDist, IterTr, IterTc] ->
        (FromRow == IterFr, FromCol == IterFc ->
            (Distance < IterDist ->
                TempMovesFromPosition1 = [[FromRow, FromCol, ToRow, ToCol, 0]],
                filter_smalles_moves(Rest, [FromRow, FromCol, Distance, ToRow, ToCol], TempMovesFromPosition1, FilteredMovesResult, FilteredMoves)
            ;
            Distance == IterDist ->
                append([[FromRow, FromCol, ToRow, ToCol, 0]], TempMovesFromPosition, TempMovesFromPosition1),
                filter_smalles_moves(Rest, IterMove, TempMovesFromPosition1, FilteredMovesResult, FilteredMoves)
            ;
            Distance > IterDist ->
                filter_smalles_moves(Rest, IterMove, TempMovesFromPosition, FilteredMovesResult, FilteredMoves)
            )
        ;
        append(TempMovesFromPosition, FilteredMovesResult, FilteredMovesResult1),
        filter_smalles_moves(Rest, [FromRow, FromCol, Distance, ToRow, ToCol], [[FromRow, FromCol, Distance, ToRow, ToCol]], FilteredMovesResult1, FilteredMoves)
        )
    ;
    filter_smalles_moves(Rest, [FromRow, FromCol, Distance, ToRow, ToCol], [[FromRow, FromCol, ToRow, ToCol, 0]], FilteredMoves)
    ).

isolated_moves(FromRow, FromCol, ToRow, ToCol, BoardSize) :-
    adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
    within_bounds(ToRow, ToCol, BoardSize).


xor(A, B) :-
    (A, \+ B) ; (\+ A, B) ; (\+ A, \+ B).

get_smallest_path([Fr, Fc, ToRow, ToCol],AllPieces, Distance , Dest) :-
    findall([DestRow, DestCol, TempDistance],
            (member([DestRow, DestCol], AllPieces),
             xor((Fr == DestRow), (Fc == DestCol)),
             distance_between(ToRow, ToCol, DestRow, DestCol, TempDistance)),
            Distances),
    (Distances = [] ->
        Dest = [ToRow, ToCol], Distance = 0
    ;
        SmallestDistances = [[_,_,1000]],
        find_min_distance(Distances, SmallestDistances,FinalSmallestDistances),
        Dest = FinalSmallestDistances
    ).

find_min_distance([], SmallestDistances, SmallestDistances).
find_min_distance([[DestRow, DestCol, Distance] | Rest], [CurrSmallestNode | RestSmallest], Result) :-
    CurrSmallestNode = [_, _, CurrSmallestDistance],

    (Distance < CurrSmallestDistance ->
        TempSmallestDistances = [[DestRow, DestCol, Distance]],
        find_min_distance(Rest, TempSmallestDistances, Result)
    ; Distance == CurrSmallestDistance ->
        append([[DestRow, DestCol, Distance]], [CurrSmallestNode], TempSmallestDistances),
        find_min_distance(Rest, TempSmallestDistances, Result)
    ;
        find_min_distance(Rest, [CurrSmallestNode | RestSmallest], Result)
    ).
    
distance_between(FromRow, FromCol, ToRow, ToCol, Distance) :-
    DeltaRow is ToRow - FromRow,
    DeltaCol is ToCol - FromCol,
    EuclideanDistance is sqrt(DeltaRow * DeltaRow + DeltaCol * DeltaCol),
    Distance is floor(EuclideanDistance).

%[Board, CurrentPlayer, BoardSize, Player1Points, Player2Points, Player1Type, Player2Type]
non_isolated_moves(FromRow, FromCol, ToRow, ToCol, StackPosition, [Board, CurrentPlayer, BoardSize, _, _, _, _]):-
    %check is not empty
    adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
    within_bounds(ToRow, ToCol, BoardSize),
    is_not_empty(Board, [ToRow, ToCol]),
    get_stack_pieces(Board, FromRow, FromCol, StackPieces),
    valid_merge(Board, StackPieces, [FromRow,FromCol,ToRow, ToCol], StackPosition, CurrentPlayer),
    StackPosition \= [].


move([Board, CurrentPlayer, BoardSize, _, _, _, _], [FromRow, FromCol, ToRow, ToCol, Index], [NewBoard, CurrentPlayer, BoardSize,_,_,_,_]) :-
    ( Index =\= 0 ->
        merge_stacks(Board, FromRow, FromCol, ToRow, ToCol, Index, NewBoard),
        write('Stack merged successfully!'), nl
        
    ; 
        adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol),
        get_stack(Board, FromRow, FromCol, Stack),
        set_piece_for_basicmove(Board, FromRow, FromCol, [], TempBoard),
        set_piece_for_basicmove(TempBoard, ToRow, ToCol, Stack, NewBoard),
        write('Stack moved successfully!'), nl
        
    ).

set_piece_for_basicmove(Board, Row, Col, Piece, UpdatedBoard) :-
    nth1(Row, Board, BoardRow, RestRows),
    nth1(Col, BoardRow, _, RestCols),
    nth1(Col, NewBoardRow, Piece, RestCols),
    nth1(Row, UpdatedBoard, NewBoardRow, RestRows).


get_stack(Board, Row, Col, Stack) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Stack).

set_piece_for_merge(Board, Row, Col, Piece, UpdatedBoard) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    append([], Piece, NewCell), 
    set_row(BoardRow, Col, NewCell, NewRow),
    set_row(Board, Row, NewRow, UpdatedBoard).

merge_stacks(Board, FromRow, FromCol, ToRow, ToCol, Index, NewBoard) :-
    get_stack(Board, FromRow, FromCol, FromStack),
    get_stack(Board, ToRow, ToCol, ToStack),
    
    % Merge stacks at the given index
    IndexMinusOne is Index - 1,
    length(Keep, IndexMinusOne),
    append(Keep, ToMerge, FromStack),
    
    % Merge ToMerge from FromStack into ToStack
    append(ToStack, ToMerge, NewToStack),
    
    % Update the FromStack with only the bottom part
    NewFromStack = Keep,
    
    % Update the board with the new stacks
    set_piece_for_merge(Board, FromRow, FromCol, NewFromStack, TempBoard),
    set_piece_for_merge(TempBoard, ToRow, ToCol, NewToStack, NewBoard).



% Helper predicate to update the board with a new stack at a specific position
update_board(Board, Row, Col, NewStack, NewBoard) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, _, RestRow),
    nth1(Col, NewRow, NewStack, RestRow),
    nth1(Row, Board, _, RestBoard),
    nth1(Row, NewBoard, NewRow, RestBoard).




valid_merge(Board,StackPieces, [FromRow, FromCol, ToRow, ToCol], StackPosition, CurrentPlayer) :-
    get_stack_pieces(Board, ToRow, ToCol, ToStack),
    length(ToStack, ToStackLength),

    findall(Index,
            (nth1(Index, StackPieces, Piece),
             Piece == CurrentPlayer,
             length(StackPieces, StackPiecesLength),
             TotalLength is ToStackLength + (StackPiecesLength - Index +1 ),
             TotalLength =< 8,
             Index =< ToStackLength),
            StackPosition).

get_stack_pieces(Board, Row, Col, StackPieces) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Stack),
    Stack \= [],
    StackPieces = Stack.


    

is_not_empty(Board, [ToRow, ToCol]) :-
    nth1(ToRow, Board, BoardRow),
    nth1(ToCol, BoardRow, Stack),
    Stack \= [].



is_stack_isolated(Board, BoardSize, Row, Col) :-
    
    % Check top-left diagonal
    (Row1 is Row - 1, Col1 is Col - 1,
     (within_bounds(Row1, Col1, BoardSize) ->
        nth1(Row1, Board, BoardRow1),
        nth1(Col1, BoardRow1, Stack1),
        Stack1 == []
     ; true)),
    % Check top-right diagonal
    (Row2 is Row - 1, Col2 is Col + 1,
     (within_bounds(Row2, Col2, BoardSize) ->
        nth1(Row2, Board, BoardRow2),
        nth1(Col2, BoardRow2, Stack2),
        Stack2 == []
     ; true)),
    % Check bottom-left diagonal
    (Row3 is Row + 1, Col3 is Col - 1,
     (within_bounds(Row3, Col3, BoardSize) ->
        nth1(Row3, Board, BoardRow3),
        nth1(Col3, BoardRow3, Stack3),
        Stack3 == []
     ; true)),
    % Check bottom-right diagonal
    (Row4 is Row + 1, Col4 is Col + 1,
     (within_bounds(Row4, Col4, BoardSize) ->
        nth1(Row4, Board, BoardRow4),
        nth1(Col4, BoardRow4, Stack4),
        Stack4 == []
     ; true)).

% Helper predicate to check if coordinates are within bounds
within_bounds(Row, Col, BoardSize) :-
    Row >= 1, Row =< BoardSize,
    Col >= 1, Col =< BoardSize.


adjacent_position_diagonal(FromRow, FromCol, ToRow, ToCol) :-
    (ToRow is FromRow - 1, ToCol is FromCol - 1); % Top-left
    (ToRow is FromRow - 1, ToCol is FromCol + 1); % Top-right
    (ToRow is FromRow + 1, ToCol is FromCol - 1); % Bottom-left
    (ToRow is FromRow + 1, ToCol is FromCol + 1). % Bottom-right


