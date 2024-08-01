:- initialization(main).

main :-
    read_file('input.txt', Masses),
    findall(Fuel, (member(Mass, Masses), fuel(Mass, Fuel)), Fuels),
    sum_list(Fuels, TotalFuel),
    format('Total fuel requirement: ~w~n', [TotalFuel]).

read_file(File, Masses) :-
    open(File, read, Stream),
    read_lines(Stream, Masses),
    close(Stream).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [Mass|Masses]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    number_string(Mass, Line),
    read_lines(Stream, Masses).

fuel(Mass, Fuel) :-
    Fuel is max(0, (Mass // 3) - 2).