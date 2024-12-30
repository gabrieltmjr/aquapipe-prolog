:- include('game.pl').

/*
Menu

All variables are singleton, because they are to be defined by the user:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players - Can be h/h, h/pc, pc/h, pc/pc, where h -> Human, pc -> Computer
Difficulty - represents the difficulty of the PC, it can be Random or Greedy
Option1, Option2, Option3 - Variables used for user input
*/

menu(0, Mode, Players, Difficulty) :-
    write('1. Play'), nl,
    write('2. Quit'), nl,
    read(Option),
    menu(Option, Mode, Players, Difficulty).

menu(1, Mode, F/S, Difficulty) :- % Play mode
    write('Choose the game mode you want to play, by writing a number between 1 and 2:'), nl,
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
    playerMode(Option2, F/S, Difficulty),
    initial_state(Mode-F/S-Difficulty, GameState),
    game_loop(F, GameState).
    %display_game(GameState).

menu(2, _, _, _). % Exit

gameMode(1, '3x3').
gameMode(2, '4x4').

playerMode(1, h/h, none).

playerMode(Option, Players, Difficulty) :-
    playerModeOp(Option, Players),
    write('Choose the difficulty of the PC, by writing a number between 1 and 3:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Minimax'), nl,
    read(Option3),
    difficulty(Option3, Difficulty).

playerModeOp(2, h/pc).
playerModeOp(3, pc/h).
playerModeOp(4, pc/pc).

difficulty(1, random).
difficulty(2, greedy).
difficulty(3, minimax).