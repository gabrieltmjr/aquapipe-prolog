:- use_module(library(lists)), use_module(library(random)).

/*
Piece Representation: CurrentPlayer-PlayerColor-PipeType-PipeNumber-InBoard-RowInBoard/ColInBoard-RowInBoardUPipe-ColInBoardUPipe

CurrentPlayer: player (h or pc).
PlayerColor: color of the CurrentPlayer (blue or red)
Pipe (PipeType/PipeNumber): type of pipe that was placed/moved (s, m, l, mup or lup) and its index (1, 2, 3)
InBoard: boolean value that represents if piece is in board or not, false meaning out of board, true meaning in the board
RowInBoard & ColInBoard : If piece is in board, represents the row and column where it is, otherwise n/n 
RowInBoardUPipe & ColInBoardUPipe : If piece is U pipe, represents the row and column where the other end of the U pipe is, otherwise n/n (only applicable with 4x4 version)

Move Representation: Piece-DRow/DCol-DRowUPipe/DColUpipe, where:

Piece: represents the piece to be moved
DRow & DRow: destination position in the board where the piece will be placed.
DRowUPipe & DColUPipe : If piece is U pipe, represents the row and column where the other end of the U pipe will be placed (applicable if U pipe, n/n otherwise)

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

% if piece in board
move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Player-Color-PipeType-PipeNumber-true-SrcRow/SrcCol-n/n-DestRow/DestCol-n/n, 
    '3x3'-F-CF-PF/S-CS-PS-Level-NewBoard-CurrentPlayer-PlayerColor-NewPlayerPieces-PossibleMoves) :-
    nth1(SrcRow, Board, RowForE), % Get row to replace with e
    nth1(SrcCol, RowForE, ColForE), % Get col to replace with e
    nth1(DestRow, Board, Row_), % Get row to change
    nth1(DestCol, Row_, Col_), % Get pos to change
    pipe('3x3', PipeType, PipeIndex), % Get PipeIndex
    add_piece_to_board(ColForE, RowForE, Board, SrcCol, SrcRow, e, PipeIndex, AuxBoard), % Add e to board
    add_piece_to_board(Col_, Row_, AuxBoard, DestCol, DestRow, Player-Color-PipeType-PipeNumber-true, PipeIndex, NewBoard), !, % Cut - no backtrack after moving (because of game_over)
    nth0(Pos, PlayerPieces, Player-Color-PipeType-PipeNumber-true-SrcRow/SrcCol-n/n, RemainingPieces), % Remove old piece
    nth0(Pos, NewPlayerPieces, Player-Color-PipeType-PipeNumber-true-DestRow/DestCol-n/n, RemainingPieces). % Update piece position in board

% If piece not in board
move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Player-Color-PipeType-PipeNumber-false-n/n-n/n-DestRow/DestCol-n/n, 
    '3x3'-F-CF-PF/S-CS-PS-Level-NewBoard-CurrentPlayer-PlayerColor-NewPlayerPieces-PossibleMoves) :-
    nth1(DestRow, Board, Row_), % Get row to change
    nth1(DestCol, Row_, Col_), % Get pos to change
    pipe('3x3', PipeType, PipeIndex), % Get PipeIndex
    add_piece_to_board(Col_, Row_, Board, DestCol, DestRow, Player-Color-PipeType-PipeNumber-false, PipeIndex, NewBoard), !, % Cut - no backtrack after moving (because of game_over)
    nth0(Pos, PlayerPieces, Player-Color-PipeType-PipeNumber-false-n/n-n/n, RemainingPieces), % Remove old piece
    nth0(Pos, NewPlayerPieces, Player-Color-PipeType-PipeNumber-true-DestRow/DestCol-n/n, RemainingPieces). % Add new piece to say its in board and with position in board
/*

choose_move(+GameState, +Level, -Move). 

This predicate receives the current game state and 
returns the move chosen by the computer player. 

Level 1 should return a random valid move. 
Level 2 should return the best play at the time (using a greedy algorithm), 
considering the evaluation of the game state as determined by the value/3 predicate. 

For human players, it should interact with the user to read the move.
*/

choose_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-h-PlayerColor-PlayerPieces-PossibleMoves, Level, h-PlayerColor-PipeType-PipeNumber-InBoard-SrcRow/SrcCol-n/n-DestRow/DestCol-n/n) :-
    repeat,
    format("Your pieces: ~w\n", [PlayerPieces]), nl,
    format("~w, Choose a piece to move and destination in the format: pipeType-pipeNumber-destinationRow-destinationColumn:\n", [h]),
    read(PipeType-PipeNumber-DestRow-DestCol), nl,
    member(h-PlayerColor-PipeType-PipeNumber-InBoard-SrcRow/SrcCol-n/n-DestRow/DestCol-n/n, PossibleMoves).
    

% PC Level 1 - Random
choose_move('3x3'-F-CF-PF/S-CS-PS-random-Board-pc-PlayerColor-PlayerPieces-PossibleMoves, random, Move) :-
    write('pc makes a move!'), nl,
    random_member(Move, PossibleMoves).

% PC Level 2 - Greedy
% choose_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-pc-PlayerColor-PossibleMoves, 2, Move) :- 
    

/*
add_piece_to_board(+Col, +Row, +Board, +ColIndex, +RowIndex, +Piece, +PieceIndex, -NewBoard)

This predicate is responsible by adding a piece to the board by getting the spot [e,e,e],
adding the piece to it: [e, h-blue-s-1, e], then adding the spot to the row [[...],[e,h-blue-s-1,e],[...]],
then adding the row to the board: 
[
 [[e,e,e],[e,h-blue-s-1,e],[e,e,e]],
 [[e,e,e],[e,e,e],[e,e,e]],
 [[e,e,e],[e,e,e],[e,e,e]]
]

Col: column to add the Piece
Row: row to add Col (spot)
Board: Board to add Row
ColIndex: index to add the Col
RowIndex: index to add the Row
Piece: piece to be added, without the destination information
PieceIndex: index to place the Piece
NewBoard: board with change made
*/

% To add a piece
add_piece_to_board(Col, Row, Board, ColIndex, RowIndex, Player-Color-PipeType-PipeNumber-InBoard, PieceIndex, NewBoard) :-
    StartIndex is 1, % Because of using nth1
    append_at(Col, PieceIndex, StartIndex, Player-Color-PipeType-PipeNumber, [], NewList), % Add piece to the spot
    append_at(Row, ColIndex, StartIndex, NewList, [], NewRow), % Add spot to the row
    append_at(Board, RowIndex, StartIndex, NewRow, [], NewBoard). % Add row to the board

% To add an empty spot
add_piece_to_board(Col, Row, Board, ColIndex, RowIndex, e, PieceIndex, NewBoard) :-
    StartIndex is 1, % Because of using nth1
    append_at(Col, PieceIndex, StartIndex, e, [], NewList), % Add piece to the spot
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

% If at least 3 pieces of each type are in the board already for the current player, according to rules of the game
valid_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, _CP-_PC-_PT-_PN-_IB-_SR/_SC-n/n-DestRow/DestCol-n/n) :-
    member(_-_-s-_-true-_/_-_/_, PlayerPieces),
    member(_-_-m-_-true-_/_-_/_, PlayerPieces),
    member(_-_-l-_-true-_/_-_/_, PlayerPieces),
    nth0(_, PlayerPieces, _CP-_PC-_PT-_PN-_IB-_SR/_SC-n/n), % Check all pieces
    pipe('3x3', _PT, PipeIndex), % Check if empty spot matches piece type
    nth1(DestRow, Board, Row_), % Search for empty spot
    nth1(DestCol, Row_, Col_), % S
    nth1(PipeIndex, Col_, Value), % S
    Value == e. % S

% If there are not at least 3 pieces of each type in board for the current player
valid_move('3x3'-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, 
            _CP-_PC-_PT-_PN-false-n/n-n/n-DestRow/DestCol-n/n) :-
    nth0(_, PlayerPieces, _CP-_PC-_PT-_PN-false-n/n-n/n), % Get all pieces not in board
    pipe('3x3', _PT, PipeIndex), % Check if empty spot matches piece type
    nth1(DestRow, Board, Row_), % Search for empty spot
    nth1(DestCol, Row_, Col_), % S
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