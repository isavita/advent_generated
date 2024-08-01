:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    count_valid_passphrases(Lines, Count),
    format('Valid passphrases: ~w~n', [Count]).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LineString),
    string_words(LineString, Line),
    read_lines(Stream, Lines).

string_words(String, Words) :-
    split_string(String, " ", "", Words).

count_valid_passphrases(Lines, Count) :-
    include(is_valid_passphrase, Lines, ValidPassphrases),
    length(ValidPassphrases, Count).

is_valid_passphrase(Passphrase) :-
    \+ has_duplicates(Passphrase).

has_duplicates(Passphrase) :-
    findall(Word, member(Word, Passphrase), Words),
    length(Words, Length),
    list_to_set(Words, UniqueWords),
    length(UniqueWords, UniqueLength),
    Length \= UniqueLength.