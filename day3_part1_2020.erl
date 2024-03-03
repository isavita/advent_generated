-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Data), "\n", all),
    FilteredLines = lists:filter(fun(Line) -> Line =/= "" end, Lines),
    TreesEncountered = count_trees(FilteredLines, 0, 3, 0), % Lines, StartPosition, RightSteps, TreeCount
    io:format("~p~n", [TreesEncountered]).

count_trees([], _, _, Count) -> Count;
count_trees([Line | Rest], Position, RightSteps, Count) ->
    LineLength = length(Line),
    % Calculate new position, wrapping around if necessary
    NewPosition = (Position + RightSteps) rem LineLength,
    % Check if the current position is a tree
    IsTree = case lists:nth(Position + 1, Line) of
                 $# -> 1;
                 _ -> 0
             end,
    count_trees(Rest, NewPosition, RightSteps, Count + IsTree).
