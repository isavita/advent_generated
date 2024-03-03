-module(task).
-export([call/0]).

call() ->
    {HPos, Depth, Aim} = {0, 0, 0},
    {FinalHPos, FinalDepth, _FinalAim} = process_commands("input.txt", {HPos, Depth, Aim}),
    Answer = FinalHPos * FinalDepth,
    io:format("~p~n", [Answer]).

process_commands(File, {HPos, Depth, Aim}) ->
    case file:read_file(File) of
        {ok, Data} ->
            Commands = string:tokens(binary:bin_to_list(Data), "\n"),
            {FinalHPos, FinalDepth, _FinalAim} = lists:foldl(fun(Command, {HPosAcc, DepthAcc, AimAcc}) ->
                {HPosStep, DepthStep, AimStep} = case string:tokens(Command, " ") of
                    ["forward", X] -> {list_to_integer(X), 0, 0};
                    ["down", X] -> {0, 0, list_to_integer(X)};
                    ["up", X] -> {0, 0, -list_to_integer(X)};
                    _ -> {0, 0, 0}
                end,
                {HPosAcc + HPosStep, DepthAcc + DepthStep + AimAcc * HPosStep, AimAcc + AimStep}
            end, {HPos, Depth, Aim}, Commands),
            {FinalHPos, FinalDepth, Aim};
        _ ->
            {HPos, Depth, Aim}
    end.