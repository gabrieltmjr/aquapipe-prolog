:- include('value.pl').

% Find the maximum value in a list
max_list([X], X).
max_list([X | Xs], Max) :-
    max_list(Xs, TailMax),
    Max is max(X, TailMax).

% Find the minimum value in a list
min_list([X], X).
min_list([X | Xs], Min) :-
    min_list(Xs, TailMin),
    Min is min(X, TailMin).

% minimax(+GameState, +Depth, +MaximizingPlayer)
% If the node is a terminal node (GameState indicates a Game Over), or Depth is 0, the base case is reached.
% The value of the given GameState is returned.
% If the game is over, then we hope the score will be extreme enough to indicate a very favorable or very unfavorable position
% for the PC, thanks to the value criteria defined in 'value.pl'.
minimax(Player, GameState, Depth, MaximizingPlayer, Value) :-
    ( Depth = 0 ; game_over(GameState, Result) ),
    value(GameState, Player, Value).

minimax(Player, GameState, Depth, true, Value) :-
    Depth > 0,
    valid_moves(GameState, Moves),
    NewDepth is Depth - 1,
    /* findall(ChildValue,
            (
                member(Move, PossibleMoves),
                move(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Move, NewGameState),
                minimax(NewGameState, NewDepth, false, ChildValue)
            ),
            ChildValues), */
    maplist(minimax_move(
        Player,
        NewGameState,
        NewDepth,
        false), Moves, ChildValues),
    max_list(ChildValues, Value).

minimax(Player, GameState, Depth, false, Value) :-
    Depth > 0,
    valid_moves(GameState, Moves),
    NewDepth is Depth - 1,
    /* findall(ChildValue,
            (
                member(Move, PossibleMoves),
                move(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Move, NewGameState),
                minimax(NewGameState, NewDepth, true, ChildValue)
            ),
            ChildValues), */
    maplist(minimax_move(
        Player,
        NewGameState,
        NewDepth,
        true), Moves, ChildValues),
    min_list(ChildValues, Value).

minimax_move(Player, GameState, Depth, MaximizingPlayer, Move, Value) :-
    move(GameState, Move, NewGameState),
    next_player(NewGameState, ReadyGameState),
    minimax(Player, NewGameState, Depth, MaximizingPlayer, Value).