% Read the input from a file
read_input(File, Digits) :-
    open(File, read, Stream),
    read_string(Stream, _, String),
    close(Stream),
    atom_codes(String, Codes),
    maplist(code_to_digit, Codes, Digits).

code_to_digit(Code, Digit) :-
    Char is Code,
    char_type(Char, digit(Digit)).

% Part One: Sum of digits that match the next digit
solve_part_one(Digits, Sum) :-
    length(Digits, Length),
    findall(D, (nth1(I, Digits, D), J is (I mod Length) + 1, nth1(J, Digits, D)), Matches),
    sum_list(Matches, Sum).

% Part Two: Sum of digits that match the digit halfway around the circular list
solve_part_two(Digits, Sum) :-
    length(Digits, Length),
    Halfway is Length // 2,
    findall(D, (nth1(I, Digits, D), J is (I + Halfway - 1) mod Length + 1, nth1(J, Digits, D)), Matches),
    sum_list(Matches, Sum).

% Main predicate to read input and solve both parts
main :-
    read_input('input.txt', Digits),
    solve_part_one(Digits, Sum1),
    solve_part_two(Digits, Sum2),
    write('Part One: '), writeln(Sum1),
    write('Part Two: '), writeln(Sum2).

% Run the main predicate
:- initialization(main).