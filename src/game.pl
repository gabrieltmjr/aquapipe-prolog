/*
Game

Game Configuration Representation: Mode-Players-Difficulty
where,

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
Players (or F/S) - Can be h/h, h/pc, pc/h, pc/pc, where h -> Human, pc -> Computer
Difficulty - represents the difficulty of the PC, it can be Random, Greedy or Minimax

Game State Representation: Mode-F/S-Difficulty-Board-F-Color
where,

Mode - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
F - First Player (h or pc) with color blue
S - Second Player (h or pc) with color red
Difficulty - represents the difficulty of the PC, it can be Random, Greedy or Minimax
Board - Bi-dimensional list of 3x3 or 4x4 size, depends on Game Mode
P - Player to play on the current turn (F on the first turn)

*/

initial_state(Mode-F/S-Difficulty,
              Mode-F/S-Difficulty-[[[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]],
                                          [[e,e,e],[e,e,e],[e,e,e]]]-F) :-
    var(GameState),
    Mode == '3x3'.

initial_state(Mode-F/S-Difficulty, 
              Mode-F/S-Difficulty-[[[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e]],
                                            [[e,e,e,e],[e,e,e,e],[e,e,e,e]]]-F) :-
    var(GameState),
    Mode == '4x4'.

display_game(Mode-F/S-Difficulty-[]-Turn).
display_game(Mode-F/S-Difficulty-[Head | Tail]-Turn) :-
    write(Head), nl,
    display_game(Mode-F/S-Difficulty-Tail-Turn).

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
    next_player(NewGameState, ReadyGameState),
    game_loop(ReadyGameState).

turn(GameState, NewGameState) :-
    display_game(GameState),
    valid_moves(GameState, ListOfMoves),
    input_move(GameState, ListOfMoves, NewGameState).

next_player(Mode-F/S-Difficulty-Board-F, Mode-F/S-Difficulty-Board-S).
next_player(Mode-F/S-Difficulty-Board-S, Mode-F/S-Difficulty-Board-F).