:- initialization(main).

main :-
    read_public_keys('input.txt', CardPK, DoorPK),
    loop_size(CardPK, 7, CardLoopSize),
    loop_size(DoorPK, 7, DoorLoopSize),
    encryption_key(CardPK, DoorLoopSize, Key),
    format('Encryption key: ~w~n', [Key]).

read_public_keys(File, CardPK, DoorPK) :-
    open(File, read, Stream),
    read_line_to_string(Stream, CardPKStr),
    read_line_to_string(Stream, DoorPKStr),
    close(Stream),
    atom_number(CardPKStr, CardPK),
    atom_number(DoorPKStr, DoorPK).

loop_size(PK, Subject, LoopSize) :-
    loop_size_helper(PK, Subject, 1, 0, LoopSize).

loop_size_helper(PK, _, Value, LoopSize, LoopSize) :-
    Value =:= PK, !.
loop_size_helper(PK, Subject, Value, CurrentLoopSize, LoopSize) :-
    NextValue is (Value * Subject) mod 20201227,
    NextLoopSize is CurrentLoopSize + 1,
    loop_size_helper(PK, Subject, NextValue, NextLoopSize, LoopSize).

encryption_key(PK, LoopSize, Key) :-
    encryption_key_helper(PK, 1, LoopSize, Key).

encryption_key_helper(_, Value, 0, Value) :- !.
encryption_key_helper(PK, Value, RemainingLoops, Key) :-
    NextValue is (Value * PK) mod 20201227,
    NextRemainingLoops is RemainingLoops - 1,
    encryption_key_helper(PK, NextValue, NextRemainingLoops, Key).