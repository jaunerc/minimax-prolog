:- use_module(minimax).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(api), handle_api, []).

rest_server(Port) :-
        http_server(http_dispatch, [port(Port)]).

handle_api(Request) :-
   %format(user_output,"I'm here~n",[]),
   http_read_json(Request, DictIn,[json_object(term)]),
   %format(user_output,"Request is: ~p~n",[Request]),
   %format(user_output,"DictIn is: ~p~n",[DictIn]),
   solve(DictIn, DictOut),
   reply_json(DictOut).

:- json_object
	tictactoe(board:list).
		
solve(Query, Solution) :-
	json_to_prolog(Query, tictactoe(Board)),
	minimax(Board, BestMove),
	prolog_to_json(tictactoe(BestMove), Solution).
