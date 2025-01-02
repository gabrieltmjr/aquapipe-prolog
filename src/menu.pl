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
    write('1. Play'), nl,
    write('2. Quit'), nl,
    read(Option),
    menu(Option, Mode, Players, Level).

menu(1, Mode, F/S, Level) :- % Play mode
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
    playerMode(Option2, F-CF/S-CS, Level),
    initial_state(Mode-F-CF/S-CS-Level, GameState),
    game_loop(GameState).

menu(2, _, _, _). % Exit

gameMode(1, '3x3').
gameMode(2, '4x4').

playerMode(1, h-blue/h-red, none).

playerMode(Option, Players, Level) :-
    playerModeOp(Option, Players),
    write('Choose the level of the PC, by writing a number between 1 and 3:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('3. Minimax'), nl,
    read(Option3),
    level(Option3, Level).

playerModeOp(2, h-blue/pc-red).
playerModeOp(3, pc-blue/h-red).
playerModeOp(4, pc-blue/pc-red).

level(1, random).
level(2, greedy).
level(3, minimax).