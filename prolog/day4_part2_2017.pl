:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    count_valid_passphrases(Lines, Count1, Count2),
    format('Valid passphrases (no duplicates): ~d~n', [Count1]),
    format('Valid passphrases (no anagrams): ~d~n', [Count2]).

read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file -> Lines = []
    ; string_words(Line, Words),
      Lines = [Words | Rest],
      read_lines(Stream, Rest)
    ).

string_words(String, Words) :-
    split_string(String, " ", "", Words).

count_valid_passphrases(Lines, Count1, Count2) :-
    include(no_duplicates, Lines, Valid1),
    include(no_anagrams, Lines, Valid2),
    length(Valid1, Count1),
    length(Valid2, Count2).

no_duplicates(Words) :-
    sort(Words, Sorted),
    length(Words, L1),
    length(Sorted, L2),
    L1 =:= L2.

no_anagrams(Words) :-
    maplist(sort_word, Words, SortedWords),
    sort(SortedWords, UniqueSortedWords),
    length(SortedWords, L1),
    length(UniqueSortedWords, L2),
    L1 =:= L2.

sort_word(Word, Sorted) :-
    string_chars(Word, Chars),
    sort(Chars, SortedChars),
    string_chars(Sorted, SortedChars).