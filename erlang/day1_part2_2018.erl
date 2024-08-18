-module(task).
-export([call/0]).

call() ->
  {ok, Data} = file:read_file("input.txt"),
  Changes = [list_to_integer(X) || X <- string:tokens(binary_to_list(Data), "\n")],
  ResultingFrequency = lists:sum(Changes),
  io:format("Part 1: ~p~n", [ResultingFrequency]),
  FirstFrequencyReachedTwice = find_first_frequency_reached_twice(Changes, 0, sets:new()),
  io:format("Part 2: ~p~n", [FirstFrequencyReachedTwice]).

find_first_frequency_reached_twice(Changes, CurrentFrequency, SeenFrequencies) ->
  case sets:is_element(CurrentFrequency, SeenFrequencies) of
    true ->
      CurrentFrequency;
    false ->
      NewSeenFrequencies = sets:add_element(CurrentFrequency, SeenFrequencies),
      [FirstChange | RestChanges] = Changes,
      NewCurrentFrequency = CurrentFrequency + FirstChange,
      find_first_frequency_reached_twice(RestChanges ++ [FirstChange], NewCurrentFrequency, NewSeenFrequencies)
  end.