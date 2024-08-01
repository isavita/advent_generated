:- use_module(library(clpfd)).
:- use_module(library(readutil)).

main :-
    read_file_to_lines('input.txt', Lines),
    aggregate_calories(Lines, Calories),
    sort(0, @>=, Calories, SortedCalories),
    sum_top_three(SortedCalories, Sum),
    writeln(Sum).

read_file_to_lines(File, Lines) :-
    read_file_to_string(File, String, []),
    split_string(String, "\n", "", Lines).

aggregate_calories(Lines, Calories) :-
    aggregate_calories(Lines, 0, [], Calories).

aggregate_calories([], Current, Acc, [Current|Acc]).
aggregate_calories([""|Rest], Current, Acc, Calories) :-
    aggregate_calories(Rest, 0, [Current|Acc], Calories).
aggregate_calories([Line|Rest], Current, Acc, Calories) :-
    atom_number(Line, Value),
    NewCurrent #= Current + Value,
    aggregate_calories(Rest, NewCurrent, Acc, Calories).

sum_top_three(List, Sum) :-
    length(List, Len),
    (Len >= 3 -> TopN = 3; TopN = Len),
    length(TopList, TopN),
    append(TopList, _, List),
    sum_list(TopList, Sum).

:- main.