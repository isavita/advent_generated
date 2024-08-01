:- initialization(main).

main :-
    read_file_to_string('input.txt', String, []),
    atom_number(String, Steps),
    find_value_after_zero(Steps, ValueAfterZero),
    format('~w', [ValueAfterZero]).

find_value_after_zero(Steps, ValueAfterZero) :-
    find_value_after_zero(Steps, 0, 0, 0, ValueAfterZero).

find_value_after_zero(_, CurrentPos, I, ValueAfterZero, ValueAfterZero) :-
    I >= 50000000, !.

find_value_after_zero(Steps, CurrentPos, I, Acc, ValueAfterZero) :-
    I1 is I + 1,
    CurrentPos1 is (CurrentPos + Steps) mod I1,
    (CurrentPos1 = 0 -> ValueAfterZero1 = I1; ValueAfterZero1 = Acc),
    CurrentPos2 is CurrentPos1 + 1,
    find_value_after_zero(Steps, CurrentPos2, I1, ValueAfterZero1, ValueAfterZero).