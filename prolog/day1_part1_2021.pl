:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_depths(Stream, Depths),
    close(Stream),
    count_increases(Depths, Increases),
    format('Number of increases: ~d~n', [Increases]).

read_depths(Stream, Depths) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file -> Depths = []
    ; number_string(Depth, Line), 
      read_depths(Stream, Rest), 
      Depths = [Depth|Rest]
    ).

count_increases([], 0).
count_increases([_], 0).
count_increases([Prev, Curr | Rest], Increases) :-
    count_increases([Curr | Rest], RestIncreases),
    ( Curr > Prev -> Increases is RestIncreases + 1
    ; Increases is RestIncreases
    ).