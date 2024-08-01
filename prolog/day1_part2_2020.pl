:- initialization(main).

main :-
    read_numbers('input.txt', Numbers),
    find_two_entries(Numbers, Product2),
    write('Product of two entries: '), write(Product2), nl,
    find_three_entries(Numbers, Product3),
    write('Product of three entries: '), write(Product3), nl.

read_numbers(File, Numbers) :-
    open(File, read, Stream),
    read_lines(Stream, Numbers),
    close(Stream).

read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [Num|Nums]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    number_string(Num, Line),
    read_lines(Stream, Nums).

find_two_entries(Numbers, Product) :-
    select(X, Numbers, Rest),
    member(Y, Rest),
    Sum is X + Y,
    Sum =:= 2020,
    Product is X * Y.

find_three_entries(Numbers, Product) :-
    select(X, Numbers, Rest1),
    select(Y, Rest1, Rest2),
    member(Z, Rest2),
    Sum is X + Y + Z,
    Sum =:= 2020,
    Product is X * Y * Z.