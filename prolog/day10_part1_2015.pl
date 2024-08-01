:- initialization(main).

% Read the input from the file
read_input(File, Input) :-
    open(File, read, Stream),
    read_line_to_string(Stream, InputString),
    close(Stream),
    string_chars(InputString, Input).

% Generate the next sequence in the look-and-say process
look_and_say([], []).
look_and_say([H|T], Result) :-
    look_and_say(T, H, 1, Result).

look_and_say([], Current, Count, Result) :-
    atomic_list_concat([Count, Current], CountCurrent),
    atom_chars(CountCurrent, CountCurrentChars),
    append(CountCurrentChars, [], Result).
look_and_say([H|T], Current, Count, Result) :-
    (H == Current ->
        NewCount is Count + 1,
        look_and_say(T, Current, NewCount, Result)
    ;
        atomic_list_concat([Count, Current], CountCurrent),
        atom_chars(CountCurrent, CountCurrentChars),
        look_and_say(T, H, 1, RestResult),
        append(CountCurrentChars, RestResult, Result)
    ).

% Apply the look-and-say transformation N times
apply_look_and_say(Input, 0, Input).
apply_look_and_say(Input, N, Result) :-
    N > 0,
    look_and_say(Input, Next),
    N1 is N - 1,
    apply_look_and_say(Next, N1, Result).

main :-
    read_input('input.txt', Input),
    apply_look_and_say(Input, 40, Result),
    length(Result, Length),
    format('Length of the result: ~d~n', [Length]).