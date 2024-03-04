-module(task).
-export([call/0]).

call() ->
    {ok, Binary} = file:read_file("input.txt"),
    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    Numbers = lists:map(fun binary_to_list/1, Lines),
    {GammaRate, EpsilonRate} = calculate_rates(Numbers),
    PowerConsumption = GammaRate * EpsilonRate,
    OxygenRating = calculate_rating(Numbers, fun most_common_bit/2, 0),
    CO2Rating = calculate_rating(Numbers, fun least_common_bit/2, 0),
    LifeSupportRating = OxygenRating * CO2Rating,
    io:format("Power Consumption: ~p~n", [PowerConsumption]),
    io:format("Life Support Rating: ~p~n", [LifeSupportRating]).

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

calculate_rating(Numbers, BitCriteria, Position) ->
    case Numbers of
        [Number] ->
            list_to_integer(Number, 2);
        _ ->
            Bit = BitCriteria(Numbers, Position),
            FilteredNumbers = lists:filter(fun(Number) -> lists:nth(Position + 1, Number) == Bit end, Numbers),
            calculate_rating(FilteredNumbers, BitCriteria, Position + 1)
    end.

most_common_bit(Numbers, Position) ->
    {Zeros, Ones} = count_bits(Numbers, Position),
    if
        Ones >= Zeros -> $1;
        true -> $0
    end.

least_common_bit(Numbers, Position) ->
    {Zeros, Ones} = count_bits(Numbers, Position),
    if
        Zeros =< Ones -> $0;
        true -> $1
    end.

count_bits(Numbers, Position) ->
    lists:foldl(fun(Number, {Zeros, Ones}) ->
                        case lists:nth(Position + 1, Number) of
                            $0 -> {Zeros + 1, Ones};
                            $1 -> {Zeros, Ones + 1}
                        end
                end, {0, 0}, Numbers).
