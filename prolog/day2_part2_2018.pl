:- initialization(main).

main :-
    read_file('input.txt', BoxIDs),
    calculate_checksum(BoxIDs, Checksum),
    format('Checksum: ~w~n', [Checksum]),
    find_similar_boxes(BoxIDs, SimilarBox),
    format('Common letters: ~w~n', [SimilarBox]).

read_file(File, BoxIDs) :-
    open(File, read, Stream),
    read_lines(Stream, BoxIDs),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

calculate_checksum(BoxIDs, Checksum) :-
    count_letters(BoxIDs, TwoCount, ThreeCount),
    Checksum is TwoCount * ThreeCount.

count_letters([], 0, 0).
count_letters([ID|Rest], TwoCount, ThreeCount) :-
    count_occurrences(ID, Occurrences),
    (member(2, Occurrences) -> TwoCount1 is 1; TwoCount1 is 0),
    (member(3, Occurrences) -> ThreeCount1 is 1; ThreeCount1 is 0),
    count_letters(Rest, TwoCountRest, ThreeCountRest),
    TwoCount is TwoCount1 + TwoCountRest,
    ThreeCount is ThreeCount1 + ThreeCountRest.

count_occurrences(ID, Occurrences) :-
    string_chars(ID, Chars),
    msort(Chars, Sorted),
    findall(Count, (adjacent_duplicates(Sorted, Count), Count > 1), Counts),
    sort(Counts, UniqueCounts),
    Occurrences = UniqueCounts.

adjacent_duplicates([], 0).
adjacent_duplicates([H|T], Count) :-
    count_adjacent(H, T, 1, Count).

count_adjacent(_, [], Count, Count).
count_adjacent(X, [X|T], Acc, Count) :-
    Acc1 is Acc + 1,
    count_adjacent(X, T, Acc1, Count).
count_adjacent(X, [Y|_], Count, Count) :-
    X \= Y.

find_similar_boxes(BoxIDs, SimilarBox) :-
    select(ID1, BoxIDs, Rest),
    member(ID2, Rest),
    differs_by_one(ID1, ID2),
    common_letters(ID1, ID2, SimilarBox).

differs_by_one(ID1, ID2) :-
    string_chars(ID1, Chars1),
    string_chars(ID2, Chars2),
    findall(Pos, (nth1(Pos, Chars1, C1), nth1(Pos, Chars2, C2), C1 \= C2), DiffPositions),
    length(DiffPositions, 1).

common_letters(ID1, ID2, Common) :-
    string_chars(ID1, Chars1),
    string_chars(ID2, Chars2),
    findall(Char, (nth1(Pos, Chars1, Char), nth1(Pos, Chars2, Char)), CommonChars),
    string_chars(Common, CommonChars).