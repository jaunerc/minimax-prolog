:- module(minimax, [minimax/2]).

% All win positions for tic-tac-toe
win_pos(P, [P, P, P, _, _, _, _, _, _]).
win_pos(P, [_, _, _, P, P, P, _, _, _]).
win_pos(P, [_, _, _, _, _, _, P, P, P]).
win_pos(P, [P, _, _, P, _, _, P, _, _]).
win_pos(P, [_, P, _, _, P, _, _, P, _]).
win_pos(P, [_, _, P, _, _, P, _, _, P]).
win_pos(P, [P, _, _, _, P, _, _, _, P]).
win_pos(P, [_, _, P, _, P, _, P, _, _]).

% full_board(+Board)
% Is true if there is no n (empty value) in the board.
full_board(Board) :-
    \+ member(n, Board).

% possible_move(+PlayerColor, +Board, -PossibleMove)
% The first n will be replaced with PlayerColor -> possible move.
possible_move(P, [n | Rest], [P | Rest]).
possible_move(P, [X | Rest], [X | Rest2]) :-
    possible_move(P, Rest, Rest2).

% all_possible_moves(+PlayerColor, +Board, -AllMoves)
% AllMoves will be matched with all possible moves for the current
% Board.
all_possible_moves(P, Board, AllMoves) :-
    findall(Move, possible_move(P, Board, Move), AllMoves).

% eval_board(+Board, -Value)
% Evaluates the score of the Board.
eval_board([], Value) :-
    Value is 0.
eval_board(Board, Value) :-
    win_pos(x, Board),
    Value is 1, !.
eval_board(Board, Value) :-
    win_pos(o, Board),
    Value is -1, !.
eval_board(Board, Value) :-
    full_board(Board),
    Value is 0.

% change_max_min(+MinOrMax, TheOther)
% Changes the MinMax atom.
change_max_min(max, min).
change_max_min(min, max).

% compare_moves(+MinMax, +MoveA, +ValueA, +MoveB, +ValueB, -BetterMove, -BetterValue)
% Chooses the move with the higher value.
compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.

% best_move(+MinMax, +AllMoves, -BestMove, -BestValue)
% Chooses the next move.
best_move(max, [], [], -2).
best_move(min, [], [], 2).
best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :-
    eval_board(Move, Value),
    best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
	compare_moves(MinMax, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).
best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :-
	best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
	change_max_min(MinMax, Other),
	minimax_step(Other, Move, _, BottomBestV),
	compare_moves(MinMax, Move, BottomBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).

% player_color(MinMax, Color)
% Matches the player color based on the MinMax atom.
player_color(max, x).
player_color(min, o).

% minimax_step(+MinMax, +Board, -BestMove, -BestValue)
% Chooses the best possible move for the current board.
minimax_step(MinMax, Board, BestMove, BestValue) :-
	player_color(MinMax, Color),
	all_possible_moves(Color, Board, AllMoves),
    best_move(MinMax, AllMoves, BestMove, BestValue).

% minimax(+Board, -BestMove)
% Matches the next move based on the current board.
minimax(Board, BestMove) :-
	minimax_step(max, Board, BestMove, _).
