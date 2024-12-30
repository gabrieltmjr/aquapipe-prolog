/*

Move Representation:

Player: player that executed the move.
Row & Col: position in the board where the move is made.
Size: size of the pipe that was placed/moved.

Therefore, the representation is: Player-Row-Col-Size

*/

:- use_module(library(lists)).

input_move(Player, Player-Row-Col-Size) :-
    validate_input('Row', Row),
    validate_input('Col', Col),
    validate_input('Size of the pipe', Size).

validate_input(Type, Input) :-
    write('Input: '), write(Type), nl,
    read(Input_),
    ((Input_ > 0 , Input_ < 4) ->
        Input = Input_;
        write('Invalid input.\n'),
        validate_input(Type, Input)
    ).

validate_move(Mode-F/S-Difficulty-Board-Turn, Player-Row-Col-Size) :-
    nth1(Row, Board, Row_, _),
    nth1(Col, Row_, Slot, _),
    nth1(Size, Slot, State, _),
    State == e.