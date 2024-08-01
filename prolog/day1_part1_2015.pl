:- initialization(main).

main :-
    read_file('input.txt', Instructions),
    calculate_floor(Instructions, Floor),
    format('Santa ends up on floor: ~d~n', [Floor]),
    halt.

read_file(File, Instructions) :-
    open(File, read, Stream),
    read_line_to_string(Stream, Line),
    close(Stream),
    string_chars(Line, Instructions).

calculate_floor(Instructions, Floor) :-
    foldl(update_floor, Instructions, 0, Floor).

update_floor('(', Acc, NewAcc) :- NewAcc is Acc + 1.
update_floor(')', Acc, NewAcc) :- NewAcc is Acc - 1.