:- use_module(library(lists)), use_module(library(random)).

/*

Move Representation: CurrentPlayer-PlayerColor-Pipe-SRow/SCol-DRow/DCol, where:

CurrentPlayer: player that executed the move.
PlayerColor: color of the CurrentPlayer
Pipe (PipeType/PipeIndex): type of pipe that was placed/moved (s, m, l, mup or lup) and its index (1, 2, 3 or 4)
DRow & DRow: destination position in the board where the move is made.
DRow2 & DCol2 : end destination position in the board where the move is made (applicable if U pipe, n otherwise)

Piece Representation: CurrentPlayer-PlayerColor-PipeType

*/

/*
pipe(?Mode, ?Pipe, ?PipeIndex)

This predicate validates a pipe.

Mode: game mode (3x3 or 4x4)
Pipe: pipe PipeType (s, m, l, mup or lup)
PipeIndex: pipe position in slot
*/
pipe(Mode, s, 1) :-
    Mode == '3x3' ; Mode == '4x4'.

pipe(Mode, m, 2) :-
    Mode == '3x3' ; Mode == '4x4'.

pipe(Mode, l, 3) :-
    Mode == '3x3' ; Mode == '4x4'.

pipe(Mode, mup, 4) :-
    Mode == '4x4'.

pipe(Mode, lup, 4) :-
    Mode == '4x4'.

% TODO:  For uniformization purposes, coordinates should start at (1,1) at the lower left corner

/*
move(+GameState, +Move, -NewGameState). 

This predicate is responsible for move validation and execution, 
receiving the current game state and the move to be executed, 
and (if the move is valid) returns the new game state after the move is executed.

*/

move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Player-Color-PipeType-PipeNumber-InBoard-DestRow/DestCol-n/n, 
    '3x3'-F-CF-PF/S-CS-PS-Level-NewBoard-CurrentPlayer-PlayerColor-NewPlayerPieces-PossibleMoves) :-
    nth1(DestRow, Board, Row_), % Get row to change
    nth1(DestCol, Row_, Col_), % Get pos to change
    pipe('3x3', PipeType, PipeIndex),
    add_piece_to_board(Col_, Row_, Board, DestCol, DestRow, Player-Color-PipeType-PipeNumber-InBoard, PipeIndex, NewBoard), !, % Cut - no backtrack after moving (because of game_over)
    nth0(Pos, PlayerPieces, Player-Color-PipeType-PipeNumber-InBoard, RemainingPieces),
    nth0(Pos, NewPlayerPieces, Player-Color-PipeType-PipeNumber-true, RemainingPieces). % Update piece to say its in board
/*

choose_move(+GameState, +Level, -Move). 

This predicate receives the current game state and 
returns the move chosen by the computer player. 

Level 1 should return a random valid move. 
Level 2 should return the best play at the time (using a greedy algorithm), 
considering the evaluation of the game state as determined by the value/3 predicate. 

For human players, it should interact with the user to read the move.
*/

choose_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-h-PlayerColor-PlayerPieces-PossibleMoves, Level, h-PlayerColor-PipeType-PipeNumber-InBoard-DestRow/DestCol-n/n) :-
    repeat,
    format("Your pieces: ~w\n", [PlayerPieces]), nl,
    format("~w, Choose a piece to move and destination in the format: pipe type-pipe number-destination row-destination column-in board value(true/false):\n", [h]),
    read(PipeType-PipeNumber-DestRow-DestCol-InBoard), nl,
    member(h-PlayerColor-PipeType-PipeNumber-InBoard-DestRow/DestCol-n/n, PossibleMoves).
    

% PC Level 1 - Random
choose_move('3x3'-F-CF-PF/S-CS-PS-random-Board-pc-PlayerColor-PlayerPieces-PossibleMoves, random, Move) :-
    write('pc makes a move!'), nl,
    random_member(Move, PossibleMoves).

% PC Level 2 - Greedy
% choose_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-pc-PlayerColor-PossibleMoves, 2, Move) :- 
    

/*
add_piece_to_board(+Col, +Row, +Board, +ColIndex, +RowIndex, +Piece, +PieceIndex, -NewBoard)

This predicate is responsible by adding a piece to the board by getting the spot [e,e,e],
adding the piece to it: [e, h-s, e], then adding the spot to the row [[...],[e,h-s,e],[...]],
then adding the row to the board: 
[
 [[e,e,e],[e,h-s,e],[e,e,e]],
 [[e,e,e],[e,e,e],[e,e,e]],
 [[e,e,e],[e,e,e],[e,e,e]]
]

Col: column to add the Piece
Row: row to add Col (spot)
Board: Board to add Row
ColIndex: index to add the Col
RowIndex: index to add the Row
Piece: piece to be added (s, m, l, mup, lup)
PieceIndex: index to place the Piece
NewBoard: board with change made
*/

add_piece_to_board(Col, Row, Board, ColIndex, RowIndex, Player-Color-PipeType-PipeNumber-InBoard, PieceIndex, NewBoard) :-
    StartIndex is 1, % Because of using nth1
    append_at(Col, PieceIndex, StartIndex, Player-Color-PipeType-PipeNumber, [], NewList), % Add piece to the spot
    append_at(Row, ColIndex, StartIndex, NewList, [], NewRow), % Add spot to the row
    append_at(Board, RowIndex, StartIndex, NewRow, [], NewBoard). % Add row to the board

/*
valid_move(+GameState, -Move)

This predicate returns a valid move, 
using singleton variables SRow and SCol to find a position.
The predicate is used by valid_moves

GameState: current state of the game
Move: valid move that can be executed

*/

% if player still has pieces out of board
valid_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, 
            _CP-_PC-_PT-_PN-false-SRow/SCol-n/n) :-
    member(_CP-_PC-_PT-_PN-false, PlayerPieces), % Check if player has pieces out of board
    nth0(_, PlayerPieces, _CP-_PC-_PT-_PN-false), % Get a piece out of board
    pipe('3x3', _PT, PipeIndex), % Check if empty spot matches piece type
    nth1(SRow, Board, Row_), % Search for empty spot
    nth1(SCol, Row_, Col_), % S
    nth1(PipeIndex, Col_, Value), % S
    Value == e. % S
    


% if all pieces are in the board already
valid_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, _CP-_PC-_PT-_PN-true-SRow/SCol-n/n) :-
    nth0(_, PlayerPieces, _CP-_PC-_PT-_PN-true), % Get a piece out of board
    pipe('3x3', _PT, PipeIndex), % Check if empty spot matches piece type
    nth1(SRow, Board, Row_), % Search for empty spot
    nth1(SCol, Row_, Col_), % S
    nth1(PipeIndex, Col_, Value), % S
    Value == e. % S


/*
valid_moves(+GameState, -ListOfMoves)

This predicate receives the current game state, 
and returns a list of all possible valid moves, 
by using the conditions of the valid_move predicate.

GameState: current state of the game
ListOfMoves: list with all valid moves
*/
valid_moves(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, ListOfMoves) :-
    findall(Move, valid_move(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Move), ListOfMoves).

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