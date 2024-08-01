:- initialization(main).

main :-
    read_file('input.txt', Capacities),
    findall(Combination, combination(Capacities, Combination), Combinations),
    include(==(150), Combinations, ValidCombinations),
    length(ValidCombinations, Count),
    format('Number of combinations: ~w~n', [Count]),
    halt.

read_file(File, Capacities) :-
    open(File, read, Stream),
    read_lines(Stream, Capacities),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [H|T]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    number_string(H, Line),
    read_lines(Stream, T).

combination(Capacities, Sum) :-
    subset(Capacities, Subset),
    sum_list(Subset, Sum).

subset([], []).
subset([H|T], [H|Subset]) :- subset(T, Subset).
subset([H|T], Subset) :- subset(T, Subset).

sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, Rest), Sum is H + Rest.