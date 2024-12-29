/*

Move Representation:

Player: player that executed the move.
Row & Col: position in the board where the move is made.
Size: size of the pipe that was placed/moved.

Therefore, the representation is: Player-Row-Col-Size

*/

input_move(Player, Player-Row-Col-Size) :-
    format("~w, input move (row, column, pipe size):\n", [Player]),
    read(Row),
    read(Col),
    read(Size).

validate_move(Mode-F/S-Difficulty-Board-Turn, Player-Row-Col-Size) :-
    nth1(Row, Board, Row_),
    nth1(Col, Row_, Slot),
    nth1(Size, Slot, State),
    State == e.