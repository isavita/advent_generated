:- initialization(main).

main :-
    read_positions(Positions),
    min_fuel_cost(Positions, MinCost),
    format('Minimum fuel cost: ~d~n', [MinCost]),
    halt.

read_positions(Positions) :-
    open('input.txt', read, Stream),
    read_line_to_string(Stream, Line),
    split_string(Line, ",", "", StringList),
    maplist(atom_number, StringList, Positions),
    close(Stream).

min_fuel_cost(Positions, MinCost) :-
    min_member(MinPos, Positions),
    max_member(MaxPos, Positions),
    findall(Cost, (between(MinPos, MaxPos, Pos), fuel_cost(Positions, Pos, Cost)), Costs),
    min_list(Costs, MinCost).

fuel_cost(Positions, Target, Cost) :-
    findall(Fuel, (member(Pos, Positions), Fuel is abs(Pos - Target)), Fuels),
    sumlist(Fuels, Cost).