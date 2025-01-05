:- include('value.pl').

% switch_player_to_move(+GameState, -NewGameState)
switch_player_to_move(Mode-F-CF-PF/S-CS-PS-Level-Board-CPlayer-PColor-CF-Moves,
                    Mode-F-CF-PF/S-CS-PS-Level-Board-CPlayer-PColor-CS-Moves).

switch_player_to_move(Mode-F-CF-PF/S-CS-PS-Level-Board-CPlayer-PColor-CS-Moves,
                    Mode-F-CF-PF/S-CS-PS-Level-Board-CPlayer-PColor-CF-Moves).
    

% minimax(+GameState, +Depth, +CurrentPlayer, +MaximizingPlayer, -BestMove, -BestValue)
% If the node is a terminal node (GameState indicates a Game Over), or Depth is 0, the base case is reached.
% The value of the given GameState is returned.
% If the game is over, then we hope the score will be extreme enough to indicate a very favorable or very unfavorable position
% for the PC, thanks to the value criteria defined in 'value.pl'.
%
% Base Case: Depth is 0 or terminal GameState has been reached: Value is returned.
minimax(GameState, Depth, CurrentPlayer, MaximizingPlayer, BestMove, BestValue) :-
    ( Depth = 0 ; game_over(GameState, Result) ), !,
    value(GameState, MaximizingPlayer, BestValue),
    BestMove = none.

% In the general recursive case, we want to find all valid moves for the Current Player given the
% current GameState, decrease the Depth and call apply_moves, to find the Best Move for that GameState.
minimax(GameState, Depth, CurrentPlayer, MaximizingPlayer, BestMove, BestValue) :-
    Depth > 0,
    valid_moves(GameState, Moves),
    NewDepth is Depth - 1,
    switch_player_to_move(GameState, NewGameState),
    next_player(NewGameState, Mode-F-CF-PF/S-CS-PS-Level-Board-Opponent-OpponentColor-_-ValidMoves),
    apply_moves(Moves,
                Mode-F-CF-PF/S-CS-PS-Level-Board-Opponent-OpponentColor-OpponentPieces-ValidMoves,
                OpponentColor,
                NewDepth,
                MaximizingPlayer,
                BestMove,
                BestValue).


inf(1000000).
neg_inf(-1000000).

% apply_moves(+ValidMoves, +GameState, +CurrentPlayer, +Depth, +MaximizingPlayer, -BestMove, -BestValue)
% Base case 1: we reached end leaf and CurrentPlayer is MaximizingPlayer. We return +inf because
% the parent Node, he MinimizingPlayer, will have to choose the minimum value of his child Nodes.
% Thus, we return +inf to be later used in a min() function.
apply_moves([], _, MaximizingPlayer, _, MaximizingPlayer, none, Inf) :-
    inf(Inf).
% Base case 2: we reached end left and CurrentPlayer is NOT MaximizingPlayer, but MinimizingPlayer.
% Now, the parent Node is the MaximizingPlayer, so it will have to choose the maximum value between
% his child Nodes. This means we return -inf, to be later used in a max() function.
apply_moves([], _, _, _, _, none, Inf) :- 
    neg_inf(Inf).

% If the CurrentPlayer is the MaximizingPlayer, then we want to find the move with the minimum value
% and pass it to our Parent Node (which is the MinimizingPlayer). Thus, for the CurrentBestValue,
% we check if it's smaller than our new Value, and keep the smallest.
% In other words, we want to give the Minimizing Player the least favorable move.
apply_moves([Move | RestOfMoves], GameState, MaximizingPlayer, Depth, MaximizingPlayer, BestMove, BestValue) :-
    move(GameState, Move, NewGameState),
    minimax(NewGameState, Depth, MaximizingPlayer, MaximizingPlayer, _, Value),
    apply_moves(RestOfMoves, GameState, MaximizingPlayer, Depth, MaximizingPlayer, CurrentBestMove, CurrentBestValue),
    ( Value < CurrentBestValue -> BestMove = Move, BestValue = Value
    ; BestMove = CurrentBestMove, BestValue = CurrentBestValue ).

% If the CurrentPlayer is not the MaximizingPlayer, then we want to find the move with the maximum value
% and pass it to our Parent Node (which is the MaximizingPlayer). Thus, for the CurrentBestValue,
% we check if it's larger than our new Value, and keep the largest.
% In other words, we want to give the MaximizingPlayer the most favorable move.
apply_moves([Move | RestOfMoves], GameState, CurrentPlayer, Depth, MaximizingPlayer, BestMove, BestValue) :-
    move(GameState, Move, NewGameState),
    minimax(NewGameState, Depth, CurrentPlayer, MaximizingPlayer, _, Value),
    apply_moves(RestOfMoves, GameState, CurrentPlayer, Depth, MaximizingPlayer, CurrentBestMove, CurrentBestValue),
    ( Value > CurrentBestValue -> BestMove = Move, BestValue = Value
    ; BestMove = CurrentBestMove, BestValue = CurrentBestValue ).
