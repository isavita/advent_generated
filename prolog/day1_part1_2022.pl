:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_lines(Stream, 0, 0, MaxCalories),
    close(Stream),
    writeln(MaxCalories).

read_lines(Stream, CurrentCalories, MaxCalories, Result) :-
    read_line_to_string(Stream, Line),
    ( Line == "" ->
        NewMaxCalories is max(CurrentCalories, MaxCalories),
        read_lines(Stream, 0, NewMaxCalories, Result)
    ; atom_number(Line, Calories) ->
        NewCurrentCalories is CurrentCalories + Calories,
        read_lines(Stream, NewCurrentCalories, MaxCalories, Result)
    ; read_lines(Stream, CurrentCalories, MaxCalories, Result)
    ).
read_lines(_, CurrentCalories, MaxCalories, MaxCalories) :- !.