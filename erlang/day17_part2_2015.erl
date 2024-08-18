-module(task).
-export([call/0]).

call() ->
  {ok, Data} = file:read_file("input.txt"),
  Containers = [list_to_integer(X) || X <- string:tokens(binary_to_list(Data), "\n")],
  {MinCount, Ways} = find_combinations(Containers, 150, 0, 0, 0, 0),
  io:format("~p~n", [Ways]).

find_combinations(_Containers, Target, _Index, _Count, MinCount, Ways) when Target == 0 ->
  {MinCount, Ways + 1};
find_combinations(_Containers, Target, _Index, _Count, MinCount, Ways) when Target < 0 ->
  {MinCount, Ways};
find_combinations(Containers, Target, Index, Count, MinCount, Ways) when Index >= length(Containers) ->
  {MinCount, Ways};
find_combinations(Containers, Target, Index, Count, MinCount, Ways) ->
  NewMinCount1 = case MinCount of
                     0 -> Count + 1;
                     _ -> MinCount
                 end,
  NewWays1 = case MinCount of
                 0 -> 1;
                 _ when Count + 1 == MinCount -> Ways + 1;
                 _ -> Ways
             end,
  {NewMinCount2, NewWays2} = find_combinations(Containers, Target - lists:nth(Index + 1, Containers), Index + 1, Count + 1, NewMinCount1, NewWays1),
  {NewMinCount3, NewWays3} = find_combinations(Containers, Target, Index + 1, Count, NewMinCount2, NewWays2),
  {min(NewMinCount2, NewMinCount3), NewWays2 + NewWays3}.