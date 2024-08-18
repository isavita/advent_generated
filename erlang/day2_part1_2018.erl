-module(task).
-export([call/0]).

call() ->
  {ok, Data} = file:read_file("input.txt"),
  Lines = string:tokens(binary_to_list(Data), "\n"),
  {TwoCount, ThreeCount} = lists:foldl(fun count_twos_and_threes/2, {0, 0}, Lines),
  Checksum = TwoCount * ThreeCount,
  io:format("~p~n", [Checksum]).

count_twos_and_threes(Line, {TwoCount, ThreeCount}) ->
  CharCount = lists:foldl(fun(Char, Acc) -> dict:update_counter(Char, 1, Acc) end, dict:new(), Line),
  {HasTwos, HasThrees} = lists:foldl(fun({Char, Count}, {Twos, Threes}) ->
                                         if Count == 2 -> {true, Threes};
                                            Count == 3 -> {Twos, true};
                                            true -> {Twos, Threes}
                                         end
                                      end, {false, false}, dict:to_list(CharCount)),
  {TwoCount + if HasTwos -> 1; true -> 0 end, ThreeCount + if HasThrees -> 1; true -> 0 end}.