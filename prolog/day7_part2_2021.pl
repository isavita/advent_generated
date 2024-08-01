:- initialization(main).

main :-
    read_positions(Positions),
    min_fuel(Positions, MinFuel),
    format('Minimum fuel required: ~w~n', [MinFuel]),
    halt.

read_positions(Positions) :-
    open('input.txt', read, Stream),
    read_line_to_string(Stream, Line),
    close(Stream),
    split_string(Line, ",", "", StringList),
    maplist(atom_number, StringList, Positions).

min_fuel(Positions, MinFuel) :-
    min_member(MinPos, Positions),
    max_member(MaxPos, Positions),
    findall(Fuel, (between(MinPos, MaxPos, Pos), fuel_cost(Positions, Pos, Fuel)), Fuels),
    min_list(Fuels, MinFuel).

fuel_cost(Positions, Target, Cost) :-
    findall(Fuel, (member(Pos, Positions), move_cost(Pos, Target, Fuel)), Costs),
    sum_list(Costs, Cost).

move_cost(Start, Target, Cost) :-
    Distance is abs(Start - Target),
    Cost is Distance * (Distance + 1) // 2.