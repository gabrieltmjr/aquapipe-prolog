:- include('game.pl').
:- include('move.pl').

/*
Menu

All variables are singleton, because they are to be defined by the user:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players - Can be h/h, h/pc, pc/h, pc/pc, where h -> Human, pc -> Computer
Level - represents the level of the PC, it can be Random, Greedy or Minimax
Option1, Option2, Option3 - Variables used for user input
*/

menu(0, Mode, Players, Level) :-
    repeat,
    write('1. Play'), nl,
    write('9. Quit'), nl,
    read(Option),
    menu(Option, Mode, Players, Level).

menu(1, Mode, F-CF-PF/S-CS-PS, Level) :- % Play mode
    repeat,
    write('Choose the game mode you want to play, by writing a number between 1 and 2:'), nl,
    write('1. 3x3'), nl,
    write('2. 3x3 - Optional Rule'), nl,
    read(Option),
    gameMode(Option, Mode),
    menu(2, Mode, F-CF-PF/S-CS-PS, Level).

menu(2, Mode, F-CF-PF/S-CS-PS, Level) :-
    repeat,
    write('Choose the player mode you want to play, by writing a number between 1 and 4:'), nl,
    write('1. H/H'), nl,
    write('2. H/PC'), nl,
    write('3. PC/H'), nl,
    write('4. PC/PC'), nl,
    read(Option2),
    playerMode(Option2, Mode, F-CF-PF/S-CS-PS, Level),
    initial_state(Mode-F-CF-PF/S-CS-PS-Level, GameState),
    game_loop(GameState),
    menu(0, NMode, NPlayers, NLevel).
    
menu(9, _, _, _). % Exit

gameMode(1, '3x3').
gameMode(2, '3x3-O').

playerMode(1, Mode, h-blue-PF/h-red-PS, none) :-
    playerPieces(Mode, h, blue, PF),
    playerPieces(Mode, h, red, PS).

playerMode(Option, Mode, Players, Level) :-
    playerModeOp(Option, Mode, Players),
    write('Choose the level of the PC, by writing a number between 1 and 3:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Minimax'), nl,
    read(Option3),
    level(Option3, Level).

playerModeOp(2, Mode, h-blue-PF/pc-red-PS) :-
    playerPieces(Mode, h, blue, PF),
    playerPieces(Mode, pc, red, PS).

playerModeOp(3, Mode, pc-blue-PF/h-red-PS) :-
    playerPieces(Mode, pc, blue, PF),
    playerPieces(Mode, h, red, PS).

playerModeOp(4, Mode, pc-blue-PF/pc-red-PS) :-
    playerPieces(Mode, pc, blue, PF),
    playerPieces(Mode, pc, red, PS).

level(1, random).
level(2, greedy).
level(3, minimax).

/*
Piece Representation: CurrentPlayer-PlayerColor-PipeType-PipeNumber-InBoard-RowInBoard/ColInBoard-RowInBoardUPipe-ColInBoardUPipe

CurrentPlayer: player (h or pc).
PlayerColor: color of the CurrentPlayer (blue or red)
Pipe (PipeType/PipeNumber): type of pipe that was placed/moved (s, m, l, mup or lup) and its index (1, 2, 3)
InBoard: boolean value that represents if piece is in board or not, false meaning out of board, true meaning in the board
RowInBoard & ColInBoard : If piece is in board, represents the row and column where it is, otherwise n/n 
RowInBoardUPipe & ColInBoardUPipe : If piece is U pipe, represents the row and column where the other end of the U pipe is, otherwise n/n (only applicable with 4x4 version)

*/

/*
playerPieces...
*/
playerPieces(Mode, P, C, [P-C-s-1-false-n/n-n/n, P-C-s-2-false-n/n-n/n, P-C-s-3-false-n/n-n/n, P-C-m-1-false-n/n-n/n, P-C-m-2-false-n/n-n/n, P-C-m-3-false-n/n-n/n, P-C-l-1-false-n/n-n/n, P-C-l-2-false-n/n-n/n, P-C-l-3-false-n/n-n/n]).