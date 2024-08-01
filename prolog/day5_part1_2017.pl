:- initialization(main).

main :-
    read_file('input.txt', Instructions),
    length(Instructions, Steps),
    jump(Instructions, 0, 0, Steps).

read_file(File, Instructions) :-
    open(File, read, Stream),
    read_lines(Stream, Instructions),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [Offset|Offsets]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    number_string(Offset, Line),
    read_lines(Stream, Offsets).

jump(Instructions, Index, Steps, Length) :-
    (   Index < 0
    ->  format('~d~n', [Steps])
    ;   Index >= Length
    ->  format('~d~n', [Steps])
    ;   nth0(Index, Instructions, Offset),
        NewIndex is Index + Offset,
        NewOffset is Offset + 1,
        replace(Instructions, Index, NewOffset, NewInstructions),
        jump(NewInstructions, NewIndex, Steps + 1, Length)
    ).

replace(List, Index, NewElem, NewList) :-
    length(Prefix, Index),
    append(Prefix, [_|Suffix], List),
    append(Prefix, [NewElem|Suffix], NewList).