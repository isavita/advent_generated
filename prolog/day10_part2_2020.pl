:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- dynamic memo/2.

% Entry point to read from file and compute results
main :-
    read_file('input.txt', Adapters),
    calculate_differences(Adapters, Result),
    write('Part 1 Result: '), write(Result), nl,
    count_arrangements(Adapters, Arrangements),
    write('Part 2 Result: '), write(Arrangements), nl.

% Read the file and convert lines to integers
read_file(File, Adapters) :-
    open(File, read, Stream),
    read_lines(Stream, Adapters),
    close(Stream).

read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [X|Xs]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    number_string(X, Line),
    read_lines(Stream, Xs).

% Calculate the differences and return the product of 1-jolt and 3-jolt differences
calculate_differences(Adapters, Result) :-
    sort(Adapters, Sorted),
    append([0], Sorted, WithOutlet),
    max_list(Sorted, Max),
    DeviceJolt is Max + 3,
    append(WithOutlet, [DeviceJolt], AllJolts),
    findall(Diff, pairwise_diff(AllJolts, Diff), Differences),
    count_differences(Differences, 1, Count1),
    count_differences(Differences, 3, Count3),
    Result is Count1 * Count3.

pairwise_diff([A, B|Rest], Diff) :- Diff is B - A, pairwise_diff([B|Rest]).
pairwise_diff([_]).  % Stop when there's one or no element left

count_differences(Differences, Value, Count) :-
    include(=(Value), Differences, Filtered),
    length(Filtered, Count).

% Count the number of distinct arrangements
count_arrangements(Adapters, Arrangements) :-
    sort(Adapters, Sorted),
    append([0], Sorted, WithOutlet),
    max_list(Sorted, Max),
    DeviceJolt is Max + 3,
    append(WithOutlet, [DeviceJolt], AllJolts),
    findall(X, count_paths(AllJolts, X), Paths),
    sumlist(Paths, Arrangements).

count_paths(Jolts, Count) :-
    valid_paths(Jolts, 0, Count).

valid_paths(Jolts, Current, Count) :-
    (   memo(Current, Count) -> true
    ;   findall(Next, (member(Next, Jolts), Next > Current, Next - Current =< 3), NextAdapters),
        (   NextAdapters \= [] 
        ->  maplist(valid_paths(Jolts), NextAdapters, Counts), 
            sumlist(Counts, Count)
        ;   Count = 1
        ),
        assertz(memo(Current, Count))
    ).

:- initialization(main).