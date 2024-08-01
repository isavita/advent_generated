:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_commands(Stream, 0, 0, HPos, Depth),
    close(Stream),
    Result is HPos * Depth,
    format('Final horizontal position: ~d~n', [HPos]),
    format('Final depth: ~d~n', [Depth]),
    format('Result (horizontal * depth): ~d~n', [Result]).

read_commands(Stream, HPos, Depth, FinalHPos, FinalDepth) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file ->
        FinalHPos = HPos,
        FinalDepth = Depth
    ; 
        process_command(Line, HPos, Depth, NewHPos, NewDepth),
        read_commands(Stream, NewHPos, NewDepth, FinalHPos, FinalDepth)
    ).

process_command(Command, HPos, Depth, NewHPos, NewDepth) :-
    split_string(Command, " ", "", [Action, ValueStr]),
    number_string(Value, ValueStr),
    ( Action == "forward" -> 
        NewHPos is HPos + Value, 
        NewDepth is Depth
    ; Action == "down" -> 
        NewHPos is HPos, 
        NewDepth is Depth + Value
    ; Action == "up" -> 
        NewHPos is HPos, 
        NewDepth is Depth - Value
    ).