:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_line_to_string(Stream, Line),
    close(Stream),
    number_string(SerialNumber, Line),
    find_largest_3x3(SerialNumber, X, Y, MaxPower),
    format('~w,~w', [X, Y]).

power_level(X, Y, SerialNumber, Power) :-
    RackID is X + 10,
    PowerLevel is (((RackID * Y + SerialNumber) * RackID) // 100) mod 10 - 5,
    Power is PowerLevel.

grid_power(SerialNumber, X, Y, TotalPower) :-
    findall(P, (between(0, 2, DX), between(0, 2, DY),
                 X1 is X + DX, Y1 is Y + DY,
                 power_level(X1, Y1, SerialNumber, P)), Powers),
    sum_list(Powers, TotalPower).

find_largest_3x3(SerialNumber, X, Y, MaxPower) :-
    findall((TotalPower, X1, Y1), (between(1, 298, X1), between(1, 298, Y1),
                                     grid_power(SerialNumber, X1, Y1, TotalPower)), Results),
    max_member((MaxPower, X, Y), Results).