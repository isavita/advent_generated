:- initialization(main).

main :-
    read_input(N),
    last_elf(N, Winner),
    format('The winning Elf is: ~w~n', [Winner]).

read_input(N) :-
    open('input.txt', read, Stream),
    read_line_to_string(Stream, Line),
    atom_number(Line, N),
    close(Stream).

last_elf(1, 1) :- !.
last_elf(N, Winner) :-
    K is 2,
    Remaining is N - 1,
    last_elf(Remaining, PrevWinner),
    Winner is ((PrevWinner + K - 1) mod N) + 1.