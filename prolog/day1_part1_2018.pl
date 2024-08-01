:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_frequencies(Stream, Frequencies),
    close(Stream),
    sum_list(Frequencies, ResultingFrequency),
    format('Resulting frequency: ~w~n', [ResultingFrequency]).

read_frequencies(Stream, Frequencies) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file ->
        Frequencies = []
    ; 
        number_string(Change, Line),
        read_frequencies(Stream, RestFrequencies),
        Frequencies = [Change | RestFrequencies]
    ).