:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_dimensions(Stream, Total),
    close(Stream),
    format('Total square feet of wrapping paper needed: ~d~n', [Total]).

read_dimensions(Stream, Total) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        split_string(Line, "x", "", [L, W, H]),
        number_string(Len, L),
        number_string(Wid, W),
        number_string(Hei, H),
        Area is 2 * (Len * Wid + Wid * Hei + Hei * Len),
        SmallestSide is min(Len * Wid, min(Wid * Hei, Hei * Len)),
        TotalPaper is Area + SmallestSide,
        read_dimensions(Stream, RestTotal),
        Total is TotalPaper + RestTotal
    ; Total = 0
    ).