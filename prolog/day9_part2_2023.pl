:- use_module(library(readutil)).
:- use_module(library(clpfd)).

parse_input(File, Histories) :-
    read_file_to_string(File, String, []),
    split_string(String, "\n", "", Lines),
    maplist(parse_line, Lines, Histories).

parse_line(Line, Numbers) :-
    split_string(Line, " ", "", Parts),
    maplist(number_string, Numbers, Parts).

all_zeros([]).
all_zeros([H|T]) :- H #= 0, all_zeros(T).

calculate_extrapolation([], []).
calculate_extrapolation([_], []).
calculate_extrapolation([H1, H2|T], [Diff|Rest]) :-
    Diff #= H2 - H1,
    calculate_extrapolation([H2|T], Rest).

calculate_extrapolations(History, ExtrapolationsSeries) :-
    calculate_extrapolations_helper(History, [History], ExtrapolationsSeries).

calculate_extrapolations_helper(History, Acc, ExtrapolationsSeries) :-
    all_zeros(History), 
    reverse(Acc, ExtrapolationsSeries).
calculate_extrapolations_helper(History, Acc, ExtrapolationsSeries) :-
    calculate_extrapolation(History, Extrapolations),
    calculate_extrapolations_helper(Extrapolations, [Extrapolations|Acc], ExtrapolationsSeries).

solve(Input, Result) :-
    parse_input(Input, Histories),
    foldl(calculate_prediction, Histories, 0, Result).

calculate_prediction(History, Acc, Result) :-
    calculate_extrapolations(History, ExtrapolationsSeries),
    reverse(ExtrapolationsSeries, RevExtrapolationsSeries),
    calculate_past_prediction(RevExtrapolationsSeries, 0, PastPrediction),
    Result #= Acc + PastPrediction.

calculate_past_prediction([], PastPrediction, PastPrediction).
calculate_past_prediction([[H|_]|T], PastPrediction, Result) :-
    NewPastPrediction #= H - PastPrediction,
    calculate_past_prediction(T, NewPastPrediction, Result).

main :-
    solve('input.txt', Result),
    format('~w', [Result]).

:- initialization(main).