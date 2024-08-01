:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_lines(Stream, BoxIDs),
    close(Stream),
    calculate_checksum(BoxIDs, Checksum),
    format('Checksum: ~d~n', [Checksum]).

read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LineString),
    string_chars(LineString, Line),
    read_lines(Stream, Lines).

calculate_checksum(BoxIDs, Checksum) :-
    count_twos(BoxIDs, Twos),
    count_threes(BoxIDs, Threes),
    Checksum is Twos * Threes.

count_twos([], 0).
count_twos([ID|Rest], Count) :-
    (contains_exactly_n(ID, 2) -> CountRest is 1; CountRest is 0),
    count_twos(Rest, CountOfRest),
    Count is CountRest + CountOfRest.

count_threes([], 0).
count_threes([ID|Rest], Count) :-
    (contains_exactly_n(ID, 3) -> CountRest is 1; CountRest is 0),
    count_threes(Rest, CountOfRest),
    Count is CountRest + CountOfRest.

contains_exactly_n(ID, N) :-
    letter_counts(ID, Counts),
    member(N, Counts).

letter_counts(ID, Counts) :-
    findall(L, member(L, ID), Letters),
    sort(Letters, UniqueLetters),
    findall(Count, (member(L, UniqueLetters), aggregate(count, member(L, Letters), Count)), Counts).

:- main.