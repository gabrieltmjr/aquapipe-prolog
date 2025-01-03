/*
Game

Game Configuration Representation: Mode-Players-Level, where:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players (or F-CF/S-CS) - Can be h-blue/h-red, h-blue/pc-red, pc-blue/h-red, pc-blue/pc-red, 
where h -> Human, pc -> Computer and blue/red is the color of the pieces of a player
Level - represents the level of the PC, it can be Random, Greedy or Minimax

Game State Representation: Mode-F-CF/S-CS-Level-Board-P-CP-PossibleMoves, where:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
F & CF  - First Player, F (h or pc) with color blue (CF - Color F)
S & CS - Second Player, S (h or pc) with color red (CS - Color S)
Level - represents the level of the PC, it can be Random, Greedy or Minimax
Board - Bi-dimensional list of 3x3 or 4x4 size, depends on Game Mode
P - Player to play on the current turn (F on the first turn)
CP - Color of player P
PossibleMoves - list with the moves that can be made by P on the current game state

*/

initial_state(Mode-F-CF/S-CS-Level,
              Mode-F-CF/S-CS-Level-[[[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]]]-F-CF-PossibleMoves) :-
    var(GameState),
    Mode == '3x3'.

initial_state(Mode-F-CF/S-CS-Level, 
              Mode-F-CF/S-CS-Level-[[[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]]]-F-CF-PossibleMoves) :-
    var(GameState),
    Mode == '4x4'.

display_game(Mode-F-CF/S-CS-Level-[]-CurrentPlayer-PlayerColor-PossibleMoves).
display_game(Mode-F-CF/S-CS-Level-[Head | Tail]-CurrentPlayer-PlayerColor-PossibleMoves) :-
    write(Head), nl,
    display_game(Mode-F-CF/S-CS-Level-Tail-CurrentPlayer-PlayerColor-PossibleMoves).

/*
Game Loop:
1) Display the GameState
2) Ask for a move input
3) Validate move. If the move is not valid, back to 2.
4) Apply the Move, updating the GameState
5) Check for win/draw.
6) Switch turns and back to 1.
*/

game_loop(GameState) :-
    turn(GameState, NewGameState),
    next_player(NewGameState, ReadyGameState), !,
    game_loop(ReadyGameState).

turn(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, NewGameState) :-
    display_game(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves),
    value(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, CurrentPlayer, Value),
    write('Value: '),
    write(Value), nl,
    valid_moves(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, PossibleMoves),
    move(Mode-F-CF/S-CS-Level-Board-CurrentPlayer-PlayerColor-PossibleMoves, Move, NewGameState).

next_player(Mode-F-CF/S-CS-Level-Board-F-CF-PossibleMoves, Mode-F-CF/S-CS-Level-Board-S-CS-NewPossibleMoves).
next_player(Mode-F-CF/S-CS-Level-Board-S-CS-PossibleMoves, Mode-F-CF/S-CS-Level-Board-F-CF-NewPossibleMoves).

/*
game_over(+GameState, -Winner). 

This predicate receives the current game state, and verifies
whether the game is over, in which case it also identifies the winner (or draw). 
Note that this predicate should not print anything to the terminal.

*/

% game_over(GameState, Winner).

% horizontal search
% vertical search
% diagonal search