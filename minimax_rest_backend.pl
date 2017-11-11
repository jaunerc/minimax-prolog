:- use_module(minimax).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% The api will be accessed through root/minimax.
:- http_handler(root(minimax), handle_api, []).

% rest_server(+Port)
% Starts a http server instance.
rest_server(Port) :-
        http_server(http_dispatch, [port(Port)]).

% handle_api(+Request)
% Handles an http request.
handle_api(Request) :-
   http_read_json(Request, DictIn,[json_object(term)]),
   solve(DictIn, DictOut),
   reply_json(DictOut).

% JSON object for a tictactoe board.
:- json_object
	tictactoe(board:list).

% solve(+Query, -Solution)
% Matches a Solution to the given query.
solve(Query, Solution) :-
	json_to_prolog(Query, tictactoe(Board)),
	minimax(Board, BestMove),
	prolog_to_json(tictactoe(BestMove), Solution).
