:- include('game.pl').
:- include('move.pl').

/*
Piece Representation: CurrentPlayer-PlayerColor-PipeType-PipeNumber-InBoard-RowInBoard/ColInBoard

CurrentPlayer: player (h or pc).
PlayerColor: color of the CurrentPlayer (blue or red)
Pipe (PipeType/PipeNumber): type of pipe that was placed/moved (s, m, l) and its index (1, 2, 3)
InBoard: boolean value that represents if piece is in board or not, false meaning out of board, true meaning in the board
RowInBoard & ColInBoard : If piece is in board, represents the row and column where it is, otherwise n/n

*/

/*
Menu

menu(+Option, +Mode, +Players, +Level)

This predicate acts as the menu for the game

Option - One of the possible menu options, 9 being exit
Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 3x3-O (Optional Rule)
Players - Can be h/h, h/pc, pc/h, pc/pc, where h -> Human, pc -> Computer
Level - represents the level of the PC, it can be Random, Greedy or Minimax

*/

menu(0, Mode, Players, Level) :- % Main Menu
    repeat,
    write('1. Play'), nl,
    write('9. Quit'), nl,
    read(Option),
    menu(Option, Mode, Players, Level).

menu(1, Mode, F-CF-PF/S-CS-PS, Level) :- % Game Mode
    repeat,
    write('Choose the game mode you want to play, by writing a number between 1 and 2:'), nl,
    write('1. 3x3'), nl,
    write('2. 3x3 - Optional Rule'), nl,
    read(Option),
    gameMode(Option, Mode),
    menu(2, Mode, F-CF-PF/S-CS-PS, Level).

menu(2, Mode, F-CF-PF/S-CS-PS, Level) :- % Players
    repeat,
    write('Choose the player mode you want to play, by writing a number between 1 and 4:'), nl,
    write('1. H/H'), nl,
    write('2. H/PC'), nl,
    write('3. PC/H'), nl,
    write('4. PC/PC'), nl,
    read(Option2),
    playerMode(Option2, F-CF-PF/S-CS-PS, Level),
    initial_state(Mode-F-CF-PF/S-CS-PS-Level, GameState),
    game_loop(GameState),
    menu(0, NMode, NPlayers, NLevel).
    
menu(9, _, _, _). % Exit

/*
gameMode(?Mode, ?ModeName)

This predicate is used to validate the game mode choice

Mode - Number assigned to each mode
ModeName - The name of the game mode
*/

gameMode(1, '3x3').
gameMode(2, '3x3-O').

/*
playerMode(+PlayerMode, +GameMode, -Players, -Level)

This predicate assigns the Players and the Level, given the Player Mode

PlayerMode - The player mode chosen
GameMode - The game mode chosen
Players - The players with their respective colors and pieces
Level - The level of the pc, if applicable, otherwise none.

*/
playerMode(1, h-blue-PF/h-red-PS, none) :-
    playerPieces(h, blue, PF),
    playerPieces(h, red, PS).

playerMode(2, h-blue-PF/pc-red-PS, Level) :-
    readLevel(Level),
    playerPieces(h, blue, PF),
    playerPieces(pc, red, PS).

playerMode(3, pc-blue-PF/h-red-PS, Level) :-
    readLevel(Level),
    playerPieces(pc, blue, PF),
    playerPieces(h, red, PS).

playerMode(4, pc-blue-PF/pc-red-PS, Level) :-
    readLevel(Level),
    playerPieces(pc, blue, PF),
    playerPieces(pc, red, PS).

/*
readLevel(-Level)

This predicate reads the computer level the chosen by the user

Level - the level read from the user
*/
readLevel(Level) :-
    write('Choose the level of the PC, by writing a number between 1 and 3:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Minimax'), nl,
    read(Option3),
    level(Option3, Level).

/*
level(?LevelNumber, ?LevelName)

This predicate is used to validate the level choice

LevelNumber - The level number 
LevelName - The level name
*/
level(1, random).
level(2, greedy).
level(3, minimax).

/*
playerPieces(+Player, +Color, -PlayerPieces)

This predicate gives the pieces of a Player

Player - current player
Color - Color of the player 
*/
playerPieces(P, C, [P-C-s-1-false-n/n, P-C-s-2-false-n/n, P-C-s-3-false-n/n, P-C-m-1-false-n/n, P-C-m-2-false-n/n, P-C-m-3-false-n/n, P-C-l-1-false-n/n, P-C-l-2-false-n/n, P-C-l-3-false-n/n]).