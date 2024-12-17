menu(0, Mode, Players) :-
    write('1. Play'), nl,
    write('2. Quit'), nl,
    read(Option),
    menu(Option, Mode, Players).

menu(1, Mode, Players) :-
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
    playerMode(Option2, Players),
    menu(0, Mode, Players).

menu(2, _, _). /*TODO: Is this the best way to do this?*/

gameMode(1, '3x3').
gameMode(2, '4x4').

playerMode(1, 'H/H').
playerMode(2, 'H/PC').
playerMode(3, 'PC/H').
playerMode(4, 'PC/PC').