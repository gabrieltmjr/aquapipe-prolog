/*
Game

Game Configuration Representation: Mode-Players-Level, where:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players (or F-CF-PF/S-CS-PS) - Can be h-blue/h-red, h-blue/pc-red, pc-blue/h-red, pc-blue/pc-red, 
where h -> Human, pc -> Computer and blue/red is the color of the pieces of a player
Level - represents the level of the PC, it can be Random, Greedy or Minimax

Game State Representation: Mode-F-CF-PF/S-CS-PS-Level-Board-P-CP-PossibleMoves, where:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
F & CF & PF  - First Player, F (h or pc) with color blue (CF - Color F) and pieces of Player F (PF)
S & CS & PS - Second Player, S (h or pc) with color red (CS - Color S), and pieces of Player S (PS)
Level - represents the level of the PC, it can be Random, Greedy or Minimax
Board - Bi-dimensional list of 3x3 or 4x4 size, depends on Game Mode
P - Player to play on the current turn (F on the first turn)
CP - Color of player P
AvaliablePieces - the pieces that Player P can play
PossibleMoves - list with the moves that can be made by P on the current game state

*/

/*
initial_state(+GameConfig, -GameState). 

This predicate receives a desired game configuration and returns the corresponding initial game state. 
Game configuration includes the type of each player and other parameters such as board size, 
use of optional rules, player names, or other information to provide more flexibility to the game. 
The game state describes a snapshot of the current game state, including board configuration 
(typically using list of lists with different atoms for the different pieces), 
identifies the current player (the one playing next), and possibly captured pieces and/or
pieces yet to be played, or any other information that may be required, depending on the game.

*/

initial_state(Mode-F-CF-PF/S-CS-PS-Level,
              Mode-F-CF-PF/S-CS-PS-Level-[[[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]]]-F-CF-PF-PossibleMoves) :-
    var(GameState),
    Mode == '3x3'.

initial_state(Mode-F-CF-PF/S-CS-PS-Level, 
              Mode-F-CF-PF/S-CS-PS-Level-[[[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]]]-F-CF-PF-PossibleMoves) :-
    var(GameState),
    Mode == '4x4'.

/*
display_game(+GameState). 

This predicate receives the current game state (including the player who will make the next move) 
and prints the game state to the terminal. Appealing and intuitive visualizations will be valued. 
Flexible game state representations and visualization predicates will also be valued, 
for instance those that work with any board size. For uniformization purposes, 
coordinates should start at (1,1) at the lower left corner.

*/

display_game(Mode-F-CF-PF/S-CS-PS-Level-[]-CurrentPlayer-PlayerColor-PossibleMoves).
display_game(Mode-F-CF-PF/S-CS-PS-Level-[Head | Tail]-CurrentPlayer-PlayerColor-PossibleMoves) :-
    write(Head), nl,
    display_game(Mode-F-CF-PF/S-CS-PS-Level-Tail-CurrentPlayer-PlayerColor-PossibleMoves).

/*
game_loop(+GameState)

This predicate manages the GameState by allowing the players to
take turns in playing. It also checks if one of the players won the game,
or if it ended in a draw.
*/

game_loop(GameState) :-
    game_over(GameState, draw),
    nl, write('The match ends in draw!'), nl,
    display_game(GameState), nl,
    write('Back to main menu...'), nl.

game_loop(GameState) :-
    game_over(GameState, Winner),
    format("\nThe winner is: ~w\n", [Winner]),
    display_game(GameState), nl,
    write('Back to main menu...'), nl.

game_loop(GameState) :-
    turn(GameState, NewGameState),
    next_player(NewGameState, ReadyGameState), !,
    game_loop(ReadyGameState).


turn(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, NewGameState) :-
    nl, display_game(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves), nl,
    valid_moves(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, PossibleMoves),
    move(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, Move, NewGameState).

next_player(Mode-F-CF-PF/S-CS-PS-Level-Board-F-CF-PF-PossibleMoves, Mode-F-CF-PF/S-CS-PS-Level-Board-S-CS-PS-NewPossibleMoves).
next_player(Mode-F-CF-PF/S-CS-PS-Level-Board-S-CS-PS-PossibleMoves, Mode-F-CF-PF/S-CS-PS-Level-Board-F-CF-PF-NewPossibleMoves).

/*
game_over(+GameState, -Winner). 

This predicate receives the current game state, and verifies
whether the game is over, in which case it also identifies the winner (or draw). 
Note that this predicate should not print anything to the terminal.

*/
game_over(Mode-F-CF-PF/S-CS-PS-Level-Board-CurrentPlayer-PlayerColor-PlayerPieces-PossibleMoves, Winner) :-
    game_over(Board, Winner).

% vertical search
game_over([[[Player-Color-Piece, _, _],[_, _, _],[_, _, _]],
           [[Player-Color-Piece, _, _],[_, _, _],[_, _, _]],
           [[Player-Color-Piece, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, Player-Color-Piece, _],[_, _, _],[_, _, _]],
           [[_, Player-Color-Piece, _],[_, _, _],[_, _, _]],
           [[_, Player-Color-Piece, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, Player-Color-Piece],[_, _, _],[_, _, _]],
           [[_, _, Player-Color-Piece],[_, _, _],[_, _, _]],
           [[_, _, Player-Color-Piece],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _], [Player-Color-Piece, _, _], [_, _, _]],
           [[_, _, _], [Player-Color-Piece, _, _], [_, _, _]],
           [[_, _, _], [Player-Color-Piece, _, _], [_, _, _]]], Player-Color).

game_over([[[_, _, _], [_, Player-Color-Piece, _], [_, _, _]],
           [[_, _, _], [_, Player-Color-Piece, _], [_, _, _]],
           [[_, _, _], [_, Player-Color-Piece, _], [_, _, _]]], Player-Color).

game_over([[[_, _, _], [_, _, Player-Color-Piece], [_, _, _]],
           [[_, _, _], [_, _, Player-Color-Piece], [_, _, _]],
           [[_, _, _], [_, _, Player-Color-Piece], [_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[Player-Color-Piece, _, _]],
           [[_, _, _],[_, _, _],[Player-Color-Piece, _, _]],
           [[_, _, _],[_, _, _],[Player-Color-Piece, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, Player-Color-Piece, _]],
           [[_, _, _],[_, _, _],[_, Player-Color-Piece, _]],
           [[_, _, _],[_, _, _],[_, Player-Color-Piece, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, Player-Color-Piece]],
           [[_, _, _],[_, _, _],[_, _, Player-Color-Piece]],
           [[_, _, _],[_, _, _],[_, _, Player-Color-Piece]]], Player-Color).

% horizontal search
game_over([[[Player-Color-Piece, _, _],[Player-Color-Piece, _, _],[Player-Color-Piece, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, Player-Color-Piece, _],[_, Player-Color-Piece, _],[_, Player-Color-Piece, _]],
           [[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, Player-Color-Piece],[_, _, Player-Color-Piece],[_, _, Player-Color-Piece]],
           [[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, _]],
           [[Player-Color-Piece, _, _],[Player-Color-Piece, _, _],[Player-Color-Piece, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, _]],
           [[_, Player-Color-Piece, _],[_, Player-Color-Piece, _],[_, Player-Color-Piece, _]],
           [[_, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, Player-Color-Piece],[_, _, Player-Color-Piece],[_, _, Player-Color-Piece]],
           [[_, _, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]],
           [[Player-Color-Piece, _, _],[Player-Color-Piece, _, _],[Player-Color-Piece, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]],
           [[_, Player-Color-Piece, _],[_, Player-Color-Piece, _],[_, Player-Color-Piece, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, _]],
           [[_, _, Player-Color-Piece],[_, _, Player-Color-Piece],[_, _, Player-Color-Piece]]], Player-Color).

% diagonal search

game_over([[[Player-Color-Piece, _, _],[_, _, _],[_, _, _]],
           [[_, _, _],[Player-Color-Piece, _, _],[_, _, _]],
           [[_, _, _],[_, _, _],[Player-Color-Piece, _, _]]], Player-Color).

game_over([[[_, Player-Color-Piece, _],[_, _, _],[_, _, _]],
           [[_, _, _],[_, Player-Color-Piece, _],[_, _, _]],
           [[_, _, _],[_, _, _],[_, Player-Color-Piece, _]]], Player-Color).

game_over([[[_, _, Player-Color-Piece],[_, _, _],[_, _, _]],
           [[_, _, _],[_, _, Player-Color-Piece],[_, _, _]],
           [[_, _, _],[_, _, _],[_, _, Player-Color-Piece]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, _, Player-Color-Piece]],
           [[_, _, _],[_, _, Player-Color-Piece],[_, _, _]],
           [[_, _, Player-Color-Piece],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[_, Player-Color-Piece, _]],
           [[_, _, _],[_, Player-Color-Piece, _],[_, _, _]],
           [[_, Player-Color-Piece, _],[_, _, _],[_, _, _]]], Player-Color).

game_over([[[_, _, _],[_, _, _],[Player-Color-Piece, _, _]],
           [[_, _, _],[Player-Color-Piece, _, _],[_, _, _]],
           [[Player-Color-Piece, _, _],[_, _, _],[_, _, _]]], Player-Color).

% draw
game_over([[[_P1, _P4, _P7],[_P10, _P13, _P16],[_P19, _P22, _P25]],
           [[_P2, _P5, _P8],[_P11, _P14, _P17],[_P20, _P23, _P26]],
           [[_P3, _P6, _P9],[_P12, _P15, _P18],[_P21, _P24, _P27]]], draw) :-
        _P1 \= e, _P2 \= e, _P3 \= e, _P4 \= e, _P5 \= e, _P6 \= e, _P7 \= e,
        _P8 \= e, _P9 \= e, _P10 \= e, _P11 \= e, _P12 \= e, _P13 \= e, _P14 \= e,
        _P15 \= e, _P16 \= e, _P17 \= e, _P18 \= e, _P19 \= e, _P20 \= e, _P21 \= e,
        _P22 \= e, _P23 \= e, _P24 \= e, _P25 \= e, _P26 \= e, _P27 \= e.