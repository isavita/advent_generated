:- initialization(main).

main :-
    read_file('input.txt', Polymer),
    react(Polymer, Result),
    length(Result, Length),
    write(Length), nl.

read_file(File, Content) :-
    open(File, read, Stream),
    read_line_to_string(Stream, ContentString),
    close(Stream),
    string_chars(ContentString, Content).

react(Polymer, Result) :-
    react_helper(Polymer, [], Result).

react_helper([], Acc, Result) :- reverse(Acc, Result).
react_helper([H|T], Acc, Result) :-
    ( Acc = [A|_], reacts(H, A) ->
        Acc = [_|Rest], react_helper(T, Rest, Result)
    ; react_helper(T, [H|Acc], Result)
    ).

reacts(X, Y) :- 
    X \= Y,
    char_code(X, CodeX),
    char_code(Y, CodeY),
    abs(CodeX - CodeY) =:= 32.  % Check for opposite polarities