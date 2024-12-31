:- use_module(library(lists)).
/*

Move Representation:

CurrentPlayer: player that executed the move.
SRow & SCol: source position in the board where the move is made.
DRow & DCol : destination position in the board where the move is made (applicable if U pipe, n otherwise)
Pipe: size of the pipe that was placed/moved (s, m, l, mup or lup)

Therefore, the representation is: CurrentPlayer-Pipe-SRow/SCol-DRow/DCol

*/

/*
pipe(+Mode, +Pipe)

This function validates a pipe.

Mode: game mode (3x3 or 4x4)
Pipe: pipe type (s, m, l, mup or lup)
*/
pipe(Mode, s) :-
    Mode == '3x3' ; Mode == '4x4'.

pipe(Mode, m) :-
    Mode == '3x3' ; Mode == '4x4'.

pipe(Mode, l) :-
    Mode == '3x3' ; Mode == '4x4'.

pipe(Mode, mup) :-
    Mode == '4x4'.

pipe(Mode, lup) :-
    Mode == '4x4'.

/*
input_move(+GameState, +ListOfValidMoves, -NewGameState)

This predicate is responsible...tbc

*/
input_move('3x3'-F/S-Difficulty-Board-CurrentPlayer, ListOfValidMoves, '3x3'-F/S-Difficulty-NewBoard-CurrentPlayer) :-
    repeat, % repeat code until it succeeds
    format("~w, input move (pipe size, row, column):\n", [CurrentPlayer]),
    read(Pipe-SRow-SCol),
    pipe('3x3', Pipe),
    member(CurrentPlayer-Pipe-SRow/SCol-n/n, ListOfValidMoves),
    move('3x3'-F/S-Difficulty-Board-CurrentPlayer, CurrentPlayer-Pipe-SRow/SCol-n/n, '3x3'-F/S-Difficulty-NewBoard-CurrentPlayer).

/*
move(+GameState, +Move, -NewGameState). 

This predicate is responsible for move validation and execution, 
receiving the current game state and the move to be executed, 
and (if the move is valid) returns the new game state after the move is executed.

*/
move('3x3'-F/S-Difficulty-Board-CurrentPlayer, CurrentPlayer-s-SRow/SCol-n/n, '3x3'-F/S-Difficulty-NewBoard-CurrentPlayer) :-
    nth1(SRow, Board, Row_), % Get row to change
    nth1(SCol, Row_, Col_), % Get pos to change
    append_at(Col_, 1, 1, CurrentPlayer-s, [], NewList), % change pos
    append_at(Row_, SCol, 1, NewList, [], NewRow),
    append_at(Board, SRow, 1, NewRow, [], NewBoard).

move('3x3'-F/S-Difficulty-Board-CurrentPlayer, CurrentPlayer-m-SRow/SCol-n/n, '3x3'-F/S-Difficulty-NewBoard-CurrentPlayer) :-
    nth1(SRow, Board, Row_), % Get row to change
    nth1(SCol, Row_, Col_), % Get pos to change
    append_at(Col_, 2, 1, CurrentPlayer-m, [], NewList), % change pos
    append_at(Row_, SCol, 1, NewList, [], NewRow),
    append_at(Board, SRow, 1, NewRow, [], NewBoard).

move('3x3'-F/S-Difficulty-Board-CurrentPlayer, CurrentPlayer-l-SRow/SCol-n/n, '3x3'-F/S-Difficulty-NewBoard-CurrentPlayer) :-
    nth1(SRow, Board, Row_), % Get row to change
    nth1(SCol, Row_, Col_), % Get pos to change
    append_at(Col_, 3, 1, CurrentPlayer-l, [], NewList), % change pos
    append_at(Row_, SCol, 1, NewList, [], NewRow),
    append_at(Board, SRow, 1, NewRow, [], NewBoard).

/*
valid_move(+GameState, -Move)

This predicate returns a valid move, 
using singleton variables SRow and SCol to find a position.
The predicate is used by valid_moves

GameState: current state of the game
Move: valid move that can be executed

*/
valid_move('3x3'-F/S-Difficulty-Board-CurrentPlayer, CurrentPlayer-Pipe-SRow/SCol-n/n) :-
    nth1(SRow, Board, Row_),
    nth1(SCol, Row_, Col_),
    nth1(Slot, Col_, Value),
    Value == e,
    pipeCheck(Slot, Pipe).

/*
valid_moves(+GameState, -ListOfMoves)

This predicate receives the current game state, 
and returns a list of all possible valid moves, 
by using the conditions of the valid_move predicate.

GameState: current state of the game
ListOfMoves: list with all valid moves
*/
valid_moves(GameState, ListOfMoves) :-
    findall(Move, valid_move(GameState, Move), ListOfMoves).
    % write(ListOfMoves).

/*
pipeCheck(+Pos, -Pipe)

Given a position Pos, returns the Pipe type.

Pos: position in array, 1, 2 or 3.
Pipe: pipe type according to position (s-1, m-2, l-3) 
*/
pipeCheck(1, s).
pipeCheck(2, m).
pipeCheck(3, l).

/*
append_at(+List, +Index, +Count, +Elem, -AccList, -NewList)

Appends Elem to a specific Index in List.

List: list to be changed
Index: position to add Elem
Count: Starting index to count from (0 or 1, typically)
Elem: element to be added
AccList: list that accumulate values in each recursive call
NewList: list that will have the accumulated values when List is empty 
*/
append_at([], _, _, Elem, AccList, AccList).

append_at([Head | Tail], Index, Count, Elem, AccList, NewList) :-
    Index \= Count,
    Count1 is Count + 1,
    append(AccList, [Head], AccList1),
    append_at(Tail, Index, Count1, Elem, AccList1, NewList).

append_at([Head | Tail], Index, Count, Elem, AccList, NewList) :-
    Index == Count,
    Count1 is Count + 1,
    append(AccList, [Elem], AccList1),
    append_at(Tail, Index, Count1, Elem, AccList1, NewList).