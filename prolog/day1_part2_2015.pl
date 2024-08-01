:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_line_to_string(Stream, Instructions),
    close(Stream),
    calculate_floor(Instructions, Floor),
    format('Final Floor: ~w~n', [Floor]),
    find_basement_position(Instructions, BasementPosition),
    format('First Basement Position: ~w~n', [BasementPosition]).

calculate_floor(Instructions, Floor) :-
    string_chars(Instructions, Chars),
    foldl(update_floor, Chars, 0, Floor).

update_floor('(', Acc, NewFloor) :- NewFloor is Acc + 1.
update_floor(')', Acc, NewFloor) :- NewFloor is Acc - 1.

find_basement_position(Instructions, Position) :-
    string_chars(Instructions, Chars),
    find_basement(Chars, 0, 1, Position).

find_basement([], _, _, -1).
find_basement([C|_], Floor, Pos, Pos) :-
    update_floor(C, Floor, NewFloor),
    NewFloor < 0, !.
find_basement([C|Rest], Floor, Pos, BasementPos) :-
    update_floor(C, Floor, NewFloor),
    NewPos is Pos + 1,
    find_basement(Rest, NewFloor, NewPos, BasementPos).