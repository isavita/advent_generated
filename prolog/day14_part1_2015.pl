:- initialization(main).

main :-
    read_reindeer_data('input.txt', Reindeer),
    race(Reindeer, 2503, Distances),
    max_list(Distances, WinningDistance),
    format('Winning distance: ~w~n', [WinningDistance]),
    halt.

read_reindeer_data(File, Reindeer) :-
    open(File, read, Stream),
    read_lines(Stream, Reindeer),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [R|Rs]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    parse_reindeer(Line, R),
    read_lines(Stream, Rs).

parse_reindeer(Line, reindeer(Name, Speed, FlyTime, RestTime)) :-
    split_string(Line, " ", " ", Parts),
    nth0(0, Parts, Name),
    nth0(3, Parts, SpeedStr),
    nth0(6, Parts, FlyTimeStr),
    nth0(13, Parts, RestTimeStr),
    atom_number(SpeedStr, Speed),
    atom_number(FlyTimeStr, FlyTime),
    atom_number(RestTimeStr, RestTime).

race(Reindeer, Time, Distances) :-
    findall(Distance, (member(reindeer(_, Speed, FlyTime, RestTime), Reindeer), 
                      total_distance(Speed, FlyTime, RestTime, Time, Distance)), Distances).

total_distance(Speed, FlyTime, RestTime, Time, Distance) :-
    CycleTime is FlyTime + RestTime,
    FullCycles is Time // CycleTime,
    RemainingTime is Time mod CycleTime,
    DistanceCovered is FullCycles * Speed * FlyTime + min(RemainingTime, FlyTime) * Speed,
    Distance is DistanceCovered.