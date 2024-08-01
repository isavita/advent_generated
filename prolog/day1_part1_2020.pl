:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_terms(Stream, Numbers),
    close(Stream),
    find_product(Numbers, Product),
    format('The product of the entries is: ~w~n', [Product]).

read_terms(Stream, Numbers) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        number_string(Number, Line),
        read_terms(Stream, Rest),
        Numbers = [Number | Rest]
    ; Numbers = [] ).

find_product(Numbers, Product) :-
    select(X, Numbers, Rest),
    member(Y, Rest),
    Sum is X + Y,
    Sum =:= 2020,
    Product is X * Y.