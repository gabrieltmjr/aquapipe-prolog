:- include('value.pl').

% Receives a GameState, a Player and a list of all possible moves,
% and calculates for each move what value the resulting board would yield.
% Saves every result in a paired 'Move-Value' term, in the EvaluatedMoves list.

% evaluate_moves(+Player, +GameState, +PossibleMoves, +Acc, -EvaluatedMoves)
evaluate_moves(_, _, [], EvaluatedMoves, EvaluatedMoves).
evaluate_moves(Player, GameState, [Move | OtherMoves], Acc, EvaluatedMoves) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, Player, Value),
    evaluate_moves(Player, GameState, OtherMoves, [Move-Value | Acc], EvaluatedMoves).

% Receives a list of all 'Move-Value' pairs, finds the pair with the maximum 'Value',
% and returns its corresponding 'Move'.
% For each move, checks if its value is greater than that of the TempMax. If it's greater,
% then the Current move becomes the new TempMax. Otherwise, even if both are tied, the old
% TempMax stays the same (if two moves yield the same value, we don't bother in choosing which).

% best_move(+EvaluatedMoves, -Move)
best_move([FirstMove | Tail], Move) :- best_move(Tail, FirstMove, Move).
% best_move(+EvaluatedMoves, +TempMax, -Move)
best_move([], Move-Value, Move).
best_move([CurrMove-CurrValue | OtherMoves], MaxMove-MaxValue, Move) :-
    (   CurrValue > MaxValue
    ->  best_move(OtherMoves, CurrMove-CurrValue, Move)
    ;   best_move(OtherMoves, MaxMove-MaxValue, Move)).
