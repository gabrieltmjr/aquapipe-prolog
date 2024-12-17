/*
Menu

All variables are singleton, because they are to be defined by the user:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players - Can be H/H, H/PC, PC/H, PC/PC, where H is Human, PC - Computer
Difficulty - represents the difficulty of the PC, it can be Random or Greedy
Option1, Option2, Option3 - Variables used for user input

The variables NMode, NPlayers, NDifficulty, allow the user to define new parameters
for the game after playing.
*/

menu(0, Mode, Players, Difficulty) :-
    write('1. Play'), nl,
    write('2. Quit'), nl,
    read(Option),
    menu(Option, Mode, Players, Difficulty).

menu(1, Mode, Players, Difficulty) :- % Play mode
    write('Choose the game mode you want to  play, by writing a number between 1 and 2:'), nl,
    write('1. 3x3'), nl,
    write('2. 4x4'), nl,
    read(Option),
    gameMode(Option, Mode),
    write('Choose the player mode you want to play, by writing a number between 1 and 4:'), nl,
    write('1. H/H'), nl,
    write('2. H/PC'), nl,
    write('3. PC/H'), nl,
    write('4. PC/PC'), nl,
    read(Option2),
    playerMode(Option2, Players, Difficulty),
    menu(0, NMode, NPlayers, NDifficulty).

menu(2, _, _, _). % Exit

gameMode(1, '3x3').
gameMode(2, '4x4').

playerMode(1, 'H/H', 'none').

playerMode(Option, Players, Difficulty) :-
    playerModeOp(Option, Players),
    write('Choose the difficulty of the PC, by writing a number between 1 and 2:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    read(Option3),
    difficulty(Option3, Difficulty).

playerModeOp(2, 'H/PC').
playerModeOp(3, 'PC/H').
playerModeOp(4, 'PC/PC').

difficulty(1, 'random').
difficulty(2, 'greedy').