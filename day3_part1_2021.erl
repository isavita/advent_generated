-module(task).
-export([call/0]).

call() ->
    {ok, Binary} = file:read_file("input.txt"),
    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    Numbers = lists:map(fun binary_to_list/1, Lines),
    {GammaRate, EpsilonRate} = calculate_rates(Numbers),
    Answer = GammaRate * EpsilonRate,
    io:format("~p~n", [Answer]).

calculate_rates(Numbers) ->
    Length = length(hd(Numbers)),
    Counts = [{0, 0} || _ <- lists:seq(1, Length)],
    UpdatedCounts = lists:foldl(fun update_counts/2, Counts, Numbers),
    {GammaBits, EpsilonBits} = lists:unzip(lists:map(fun most_least_common/1, UpdatedCounts)),
    GammaRate = list_to_integer(GammaBits, 2),
    EpsilonRate = list_to_integer(EpsilonBits, 2),
    {GammaRate, EpsilonRate}.

update_counts(Number, Counts) ->
    lists:zipwith(fun(Bit, {Zeros, Ones}) ->
                          case Bit of
                              $0 -> {Zeros + 1, Ones};
                              $1 -> {Zeros, Ones + 1}
                          end
                  end, Number, Counts).

most_least_common({Zeros, Ones}) when Zeros > Ones -> {$0, $1};
most_least_common({Zeros, Ones}) when Ones >= Zeros -> {$1, $0}.
