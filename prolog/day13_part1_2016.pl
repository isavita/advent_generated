:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(readutil)).

main :-
    read_input(FavNumber),
    bfs([(1, 1, 0)], FavNumber, (31, 39), Visited),
    format('Fewest steps to reach (31,39): ~w~n', [Visited]).

read_input(FavNumber) :-
    open('input.txt', read, Stream),
    read_line_to_string(Stream, Line),
    atom_number(Line, FavNumber),
    close(Stream).

bfs([(X, Y, Steps) | _], _, (X, Y), Steps) :- !.
bfs([(X, Y, Steps) | Rest], FavNumber, Target, Result) :-
    findall((NX, NY, Steps1), (move(X, Y, NX, NY, FavNumber), Steps1 #= Steps + 1), NextMoves),
    subtract(NextMoves, Rest, NewMoves),
    append(Rest, NewMoves, Queue),
    bfs(Queue, FavNumber, Target, Result).

move(X, Y, NX, NY, FavNumber) :-
    member((DX, DY), [(0, 1), (1, 0), (0, -1), (-1, 0)]),
    NX #= X + DX, NY #= Y + DY,
    NX #>= 0, NY #>= 0,
    is_open(NX, NY, FavNumber).

is_open(X, Y, FavNumber) :-
    Sum #= X*X + 3*X + 2*X*Y + Y + Y*Y + FavNumber,
    num_bits(Sum, NumBits),
    NumBits mod 2 #= 0.

num_bits(N, Count) :-
    num_bits_helper(N, 0, Count).

num_bits_helper(0, Acc, Acc).
num_bits_helper(N, Acc, Count) :-
    N > 0,
    Bit #= N mod 2,
    NewAcc #= Acc + Bit,
    NewN #= N // 2,
    num_bits_helper(NewN, NewAcc, Count).

:- initialization(main).