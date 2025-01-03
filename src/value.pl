/*

Value of a Game State to a given player:

- For 3x3:
    We will count Rows/Columns/Diagonals that have 1/2/3 pipes of the same color and size, and none of the opposite color and size.
    These Rows/Columns/Diagonals can be used to win the game. If there is any pipe of same color and size of the opposite player,
    a win cannot be achieved on that line (except the opposite pipe is later moved to a different position).

    Thus, a score will be granted following this criteria:
        - 1 pipe and no enemy pipes: 1 point.
        - 2 pipes and no enemy pipes: 4 points.
        - 3 pipes and no enemy pipes (game is won): 10 points.

- For 4x4:
    Now, three of the same color and size on the same line does not win the game. Four of these, however, does.
    Moreover, a line of 4 can be achieved.
    
    Thus, we adjust the scores a little:
        - 1 pipe and no enemy pipes: 1 point.
        - 2 pipes and no enemy pipes: 2 points.
        - 3 pipes and no enemy pipes: 4 points.
        - 4 pipes and no enemy pipes (game is won): 16 points.

The score calculation will contemplate all relevant aforementioned line configurations.
The current player's lines will add a positive value to the score, and the opposite player's will add a negative value.
This means that a score of 0 is a perfectly neutral board.


For the following value calculation, we look at the following terminology:

- A Line can be a Row, Column, or Diagonal belonging to a given Board.
    For example, if we have a board looking like: 
    [[[p1,e,p2],[e,e,e],[p1,e,e]],
    [[p2,e,e],[p1,e,e],[p1,p2,e]],
    [[e,p1,p1],[p2,p2,e],[e,e,e]]]
    Then an example of a Row would be:
    Line1 = [[p1,e,p2],[e,e,e],[p1,e,e]]
    An example of a Column would be:
    Line2 = [[p1,e,p2],[p2,e,e],[e,p1,p1]]
    And an example of a Diagonal would be:
    Line3 = [[p1,e,p2],[p1,e,e],[e,e,e]]

- A Slot is, for a given Line, the list of all slots of the same size.
    For example, for the Line1 defined above, a Slot could be:
    [p1,e,p1]
    which would be the slot corresponding to the first position in each array.
    Another one is [e,e,e], and the third is [p2,e,e].
    This is calculated for every Line, because to count a potential win, we must only consider
    Lines comprised of pipes of the same size.
*/
Board1 = [[[e,e,e],[e,e,e],[e,e,e]], % Expected value for h: 0
        [[e,e,e],[e,e,e],[e,e,e]],
        [[e,e,e],[e,e,e],[e,e,e]]].
Board2 = [[[h-blue-s,e,e],[e,e,e],[e,e,e]], % Expected value for h: 3
        [[e,e,e],[e,e,e],[e,e,e]],
        [[e,e,e],[e,e,e],[e,e,e]]].
Board3 = [[[h-blue-s,e,e],[e,e,e],[pc-red-s,e,e]], % Expected value for h: 2
        [[e,e,e],[e,e,e],[e,e,e]],
        [[e,e,e],[e,e,e],[e,e,e]]].
Board4 = [[[h-blue-s,e,e],[e,e,e],[pc-red-s,e,e]], % Expected value for h: 4
        [[h-blue-s,e,e],[e,e,e],[e,e,e]],
        [[e,e,e],[e,e,e],[e,e,e]]].
Board5 = [[[h-blue-s,e,e],[e,e,e],[pc-red-s,e,e]], % Expected value for h: 2
        [[h-blue-s,e,e],[e,e,e],[e,e,e]],
        [[pc-red-s,e,e],[e,e,e],[e,e,e]]].
Board6 = [[[h-blue-s,e,e],[e,e,e],[pc-red-s,e,e]], % Expected value for h: 
        [[h-blue-s,e,e],[e,e,e],[e,e,e]],
        [[pc-red-s,e,e],[e,e,e],[e,e,e]]].


% Transposes the given Board matrix.
% cols(+Board, -Cols)
cols('3x3', Board, Cols) :-
    Board = [[A, B, C], [D, E, F], [G, H, I]],
    Cols = [[A, D, G], [B, E, H], [C, F, I]].

cols('4x4', Board, Cols) :-
    Board = [[A, B, C, D],
            [E, F, G, H],
            [I, J, K, L],
            [M, N, O, P]],
    Cols = [[A, E, I, M],
            [B, F, J, N],
            [C, G, K, O],
            [D, H, L, P]].

% diags(+Mode, +Board, -Diags)
diags('3x3', Board, [Diag1, Diag2]) :-
    Board = [[A, _, C],
            [_, E, _],
            [G, _, I]],
    Diag1 = [A, E, I],
    Diag2 = [C, E, G].

diags('4x4', Board, [Diag1, Diag2, Diag3, Diag4, Diag5, Diag6]) :-
    Board = [[A, B, C, D],
            [E, F, G, H],
            [I, J, K, L],
            [M, N, O, P]],
    Diag1 = [E, J, O],
    Diag2 = [A, F, K, P],
    Diag3 = [B, G, L],
    Diag4 = [C, F, I],
    Diag5 = [D, G, J, M],
    Diag6 = [H, K, N].

% Creates an array containing each `Line` (Row, Column, Diagonal) of a given Board.
% Each Line array is comprised of Slots, each slot is in turn an array that contains
% either a pipe, or an empty value, one for every possible pipe size.
% The Rows are the same as the Board array.
% The Cols are the array obtained when transposing the Board array.
% extract_lines(+Mode, +Board, -Lines)
extract_lines(Mode, Board, Lines) :-
    cols(Mode, Board, Cols),
    diags(Mode, Board, Diags),
    append([Board, Cols, Diags], Lines).

% Succeeds if the first part of a pipe expression Term matches Player.
% match_player(+Player, +Term)
match_player(Player, Player-_-_).

% Finds the value of a given amount of pipes of the same player, on the same slot, on the same line,
% according to the scoring mentioned above.
% compute_value(+Mode, +PipeCount, -Value)
compute_value(_, 1, 1).

compute_value('3x3', 2, 4).
compute_value('3x3', 3, 10).

compute_value('4x4', 2, 2).
compute_value('4x4', 3, 4).
compute_value('4x4', 4, 16).

:- use_module(library(lists)).

% Given a Slot, finds how many pipes belong to the player and, if there are no pipes
% belonging to the opponent, the score for that slot is calculated.
% slot_score(+Mode, +Player, +Opponent, +Slot, -Score)
slot_score(Mode, Player, Opponent, Slot, Score) :-
    include(match_player(Player), Slot, PlayerPipes),
    include(match_player(Opponent), Slot, OpponentPipes),
    length(PlayerPipes, PlayerPipeCount),
    ( OpponentPipes = [] ->
        compute_value(Mode, PlayerPipeCount, Score)
    ; Score = 0 ).

% This predicate receives a list of numbers and computes the sum of its elements.
% Its written here since for some reason the interpreter couldn't import the `sum_list/2` predicate from the lists library.
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

% For a given Line, calculates all scores of that Line's Slots, and returns the sum of those scores.
% score(+Mode, +Player, +Opponent, +Line, -Score)
score('3x3', Player, Opponent, Line, Score) :-
    Line = [[A, B, C],
            [D, E, F],
            [G, H, I]],
    AllSlots = [[A, D, G],
                [B, E, H],
                [C, F, I]],
    findall(SlotScore, (member(Slot, AllSlots), slot_score('3x3', Player, Opponent, Slot, SlotScore)), SlotScores),
    sum_list(SlotScores, Score).

score('4x4', Player, Opponent, Line, Score) :-
    Line = [[A, B, C, D],
            [E, F, G, H],
            [I, J, K, L],
            [M, N, O, P]],
    AllSlots = [[A, E, I, M],
                [B, F, J, N],
                [C, G, K, O],
                [D, H, L, P]],
    findall(SlotScore, (member(Slot, AllSlots), slot_score('4x4', Player, Opponent, Slot, Slotscore)), SlotScores),
    sum_list(SlotScores, Score).

% Calculates all values of all Lines, for both the Player and the Opponent, and then computes the value for the
% given GameState.
% value(+GameState, +Player, -Value)
value(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, Player, Value) :-
    next_player(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves,
        Mode-F-CF/S-CS-Level-Board-Opponent-OpponentColor-NewPossibleMoves),
    extract_lines(Mode, Board, Lines),
    findall(PlayerScore, (member(Line, Lines), score(Mode, Player, Opponent, Line, PlayerScore)), PlayerScores),
    findall(OpponentScore, (member(Line, Lines), score(Mode, Opponent, Player, Line, OpponentScore)), OpponentScores),
    sum_list(PlayerScores, TotalPlayerScore),
    sum_list(OpponentScores, TotalOpponentScore),
    Value is TotalPlayerScore - TotalOpponentScore.
