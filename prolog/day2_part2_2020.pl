:- initialization(main).

validate_password(Min, Max, Char, Password) :-
    nth1(Min, Password, Char1),
    nth1(Max, Password, Char2),
    (Char1 \= Char, Char2 = Char; Char1 = Char, Char2 \= Char).

count_valid_passwords(Count) :-
    open('input.txt', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    include(valid_password, Lines, ValidPasswords),
    length(ValidPasswords, Count).

valid_password(Line) :-
    split_string(Line, ":", " ", [PolicyStr, Password]),
    split_string(PolicyStr, " ", "-", [MinMaxStr, CharStr]),
    split_string(MinMaxStr, "-", "", [MinStr, MaxStr]),
    atom_number(MinStr, Min),
    atom_number(MaxStr, Max),
    string_chars(CharStr, [Char]),
    string_chars(Password, PasswordChars),
    validate_password(Min, Max, Char, PasswordChars).

read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file -> Lines = []
    ; Lines = [Line | Rest],
      read_lines(Stream, Rest)
    ).

main :-
    count_valid_passwords(Count),
    write(Count), nl.