:- dynamic light/3.

initialize_grid :-
    forall(between(0, 999, X),
           forall(between(0, 999, Y),
                  assert(light(X, Y, 0)))).

process_instruction(Line) :-
    split_string(Line, " ", "", Tokens),
    (   Tokens = ["turn", "on", Start, "through", End]
    ->  update_lights(Start, End, 1)
    ;   Tokens = ["turn", "off", Start, "through", End]
    ->  update_lights(Start, End, -1)
    ;   Tokens = ["toggle", Start, "through", End]
    ->  update_lights(Start, End, 2)
    ).

update_lights(Start, End, Change) :-
    split_string(Start, ",", "", [X1, Y1]),
    split_string(End, ",", "", [X2, Y2]),
    atom_number(X1, X1N), atom_number(Y1, Y1N),
    atom_number(X2, X2N), atom_number(Y2, Y2N),
    forall(between(X1N, X2N, X),
           forall(between(Y1N, Y2N, Y),
                  adjust_brightness(X, Y, Change))).

adjust_brightness(X, Y, Change) :-
    retract(light(X, Y, Current)),
    NewBrightness is max(Current + Change, 0),
    assert(light(X, Y, NewBrightness)).

total_brightness(Total) :-
    findall(Brightness, light(_, _, Brightness), BrightnessList),
    sum_list(BrightnessList, Total).

main :-
    initialize_grid,
    open('input.txt', read, Stream),
    repeat,
    read_line_to_string(Stream, Line),
    (   Line == end_of_file -> ! ; process_instruction(Line), fail),
    close(Stream),
    total_brightness(Total),
    format('Total brightness: ~w~n', [Total]).

:- main.