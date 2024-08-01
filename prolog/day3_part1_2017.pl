:- use_module(library(readutil)).
:- use_module(library(clpfd)).

main :-
    read_file_to_string('input.txt', String, []),
    number_string(Target, String),
    SideLength is ceiling(sqrt(Target)),
    (SideLength mod 2 =:= 0 -> SideLength1 is SideLength + 1 ; SideLength1 is SideLength),
    MaxValue is SideLength1 * SideLength1,
    StepsFromEdge is (SideLength1 - 1) // 2,
    find_distance(Target, MaxValue, StepsFromEdge, DistanceToMiddle),
    ManhattanDistance is StepsFromEdge + DistanceToMiddle,
    writeln(ManhattanDistance).

find_distance(Target, MaxValue, StepsFromEdge, Distance) :-
    findall(Dist, (between(0, 3, I),
                   MiddlePoint is MaxValue - StepsFromEdge - (MaxValue - 1) * I,
                   Dist is abs(Target - MiddlePoint)), Distances),
    min_list(Distances, Distance).

:- main.