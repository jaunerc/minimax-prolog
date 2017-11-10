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



best_move(max, [], [], -2).
best_move(min, [], [], 2).
best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :-
    eval_board(Move, Value),
    best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
	compare_moves(MinMax, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).
best_move(max, [Move | RestMoves], BestMove, BestValue) :-
	best_move(max, RestMoves, CurrentBestM, CurrentBestV),
	minimax_step(min, Move, _, BottomBestV),
	(BottomBestV > CurrentBestV ->
    BestMove = Move,
    BestValue is BottomBestV
    ;
    BestMove = CurrentBestM,
    BestValue is CurrentBestV).
best_move(min, [Move | RestMoves], BestMove, BestValue) :-
	best_move(min, RestMoves, CurrentBestM, CurrentBestV),
	minimax_step(max, Move, _, BottomBestV),
	(BottomBestV < CurrentBestV ->
    BestMove = Move,
    BestValue is BottomBestV
    ;
    BestMove = CurrentBestM,
    BestValue is CurrentBestV).

compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.




minimax_step(max, Board, BestMove, BestValue) :-
    all_possible_moves(x, Board, AllMoves),
    best_move(max, AllMoves, BestMove, BestValue).
minimax_step(min, Board, BestMove, BestValue) :-
    all_possible_moves(o, Board, AllMoves),
    best_move(min, AllMoves, BestMove, BestValue).

minimax(Board, BestMove) :-
	minimax_step(max, Board, BestMove, _).
