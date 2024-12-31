/*
Game

Game Configuration Representation: Mode-Players-Level, where:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players (or F/S) - Can be h/h, h/pc, pc/h, pc/pc, where h -> Human, pc -> Computer
Level - represents the level of the PC, it can be Random, Greedy or Minimax

Game State Representation: Mode-F/S-Level-Board-P, where:

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
F - First Player (h or pc) with color blue
S - Second Player (h or pc) with color red
Level - represents the level of the PC, it can be Random, Greedy or Minimax
Board - Bi-dimensional list of 3x3 or 4x4 size, depends on Game Mode
P - Player to play on the current turn (F on the first turn)
PossibleMoves - list with the moves that can be made by P on the current game state

*/

initial_state(Mode-F/S-Level,
              Mode-F/S-Level-[[[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]]]-F-PossibleMoves) :-
    var(GameState),
    Mode == '3x3'.

initial_state(Mode-F/S-Level, 
              Mode-F/S-Level-[[[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e],[e,e,e,e]]]-F-PossibleMoves) :-
    var(GameState),
    Mode == '4x4'.

display_game(Mode-F/S-Level-[]-Turn-PossibleMoves).
display_game(Mode-F/S-Level-[Head | Tail]-Turn-PossibleMoves) :-
    write(Head), nl,
    display_game(Mode-F/S-Level-Tail-Turn-PossibleMoves).

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

turn(Mode-F/S-Level-Board-Turn-PossibleMoves, NewGameState) :-
    display_game(Mode-F/S-Level-Board-Turn-PossibleMoves),
    valid_moves(Mode-F/S-Level-Board-Turn-PossibleMoves, PossibleMoves),
    move(Mode-F/S-Level-Board-Turn-PossibleMoves, Move, NewGameState).

next_player(Mode-F/S-Level-Board-F-PossibleMoves, Mode-F/S-Level-Board-S-NewPossibleMoves).
next_player(Mode-F/S-Level-Board-S-PossibleMoves, Mode-F/S-Level-Board-F-NewPossibleMoves).

/*
game_over(+GameState, -Winner). 

This predicate receives the current game state, and verifies
whether the game is over, in which case it also identifies the winner (or draw). 
Note that this predicate should not print anything to the terminal.

*/

game_over(GameState, Winner).

% horizontal search
% vertical search
% diagonal search