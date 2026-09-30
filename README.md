# PIK3CB
this is a pawn war UCI chess engine written purely in.
# How to play
the engine is hosted on lichess, playing under the same name PIK3CB - it accepts any challenge with a time control.
THE BOT ONLY KNOWS ABOUT PAWNS - it also expects you to play fairly and resign 1 move before it would promote - because of its non-knowledge of any other piece,
if it is played with a normal chessboard, the engine will auto-resign right before winning due to playing an illegal move.
# Name
PIK3CB is a protein.
Pik is czech chess slang for pawn,
C is for chess,
B is for (TJ) Bohemians Praha (a chess club in Prague)
# Design
The engine uses a 120-size mailbox thingy for the board. 
The search used is a minimax, as I used negamax in my other, more serious (but very shittily written) chess engine Formin-C (also written in Fortran, tho of less quality).

