:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    count_fully_contains(Lines, FullyContainsCount),
    count_overlaps(Lines, OverlapsCount),
    format('Fully Contains: ~w~n', [FullyContainsCount]),
    format('Overlaps: ~w~n', [OverlapsCount]).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

parse_range(Range, Start, End) :-
    split_string(Range, "-", " ", [StartStr, EndStr]),
    atom_number(StartStr, Start),
    atom_number(EndStr, End).

fully_contains((R1, R2)) :-
    parse_range(R1, Start1, End1),
    parse_range(R2, Start2, End2),
    Start1 =< Start2,
    End1 >= End2.

overlaps((R1, R2)) :-
    parse_range(R1, Start1, End1),
    parse_range(R2, Start2, End2),
    Start1 =< End2,
    Start2 =< End1.

count_fully_contains(Lines, Count) :-
    findall(1, (member(Line, Lines), split_string(Line, ",", " ", [R1, R2]), fully_contains((R1, R2))), Matches),
    length(Matches, Count).

count_overlaps(Lines, Count) :-
    findall(1, (member(Line, Lines), split_string(Line, ",", " ", [R1, R2]), overlaps((R1, R2))), Matches),
    length(Matches, Count).