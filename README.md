# minimax-prolog
A simple minimax implementation in SWI Prolog for Tic Tac Toe. There is also a ui client to play against this implementation under https://github.com/jaunerc/tictactoe_client

## Minimax
The minimax algorithm can be used for several games to implement a computer opponent. The file minimax.pl contributes a predicate minimax/2. Which chooses the next best move to play by a given tic tac toe board. Notice this implementation is pretty basic. It should choose the best possible move.

By example the following statement matches NextMove with the chosen move for the given board.
```Prolog
minimax([n,n,o,n,n,n,n,n,n], NextMove).
```
The board is a one dimensional list that represents a tic tac toe situation. The atom n stands for an empty field.
```
0 | 1 | 2
3 | 4 | 5
6 | 7 | 8
```
## REST backend
This repo also contains a simple REST backend. It uses only SWI Prolog standard modules. If you send a tic tac toe board you will receive a response with the computers move.

### Usage
Starts the server on port 8080.
```
?- rest_server(8080).
```

Access the running server with a http POST request on localhost:8080/minimax with a tic tac toe board as JSON value.
```
Request JSON:
{ "board":["o","n","n","n","n","n","n","n","n"]}
```
You should get a response in the same form.
```
Response JSON:
{"board": ["o","n","n","n","x","n","n","n","n"]}
```
