-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Input = string:tokens(binary_to_list(File), "\n"),
    Answer1 = lists:sum([calculate_wrapping_paper(X) || X <- Input]),
    Answer2 = lists:sum([calculate_ribbon(X) || X <- Input]),
    io:format("~p~n~p~n", [Answer1, Answer2]).

calculate_wrapping_paper(Dimensions) ->
    [L, W, H] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Dimensions, "x")),
    Sides = [L*W, W*H, H*L],
    Area = 2*lists:sum(Sides) + lists:min(Sides),
    Area.

calculate_ribbon(Dimensions) ->
    [L, W, H] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Dimensions, "x")),
    SmallestPerimeter = lists:min([2*(L+W), 2*(W+H), 2*(H+L)]),
    Volume = L*W*H,
    Ribbon = SmallestPerimeter + Volume.