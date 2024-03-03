-module(task).
-export([call/0]).

call() ->
    Answer = 31,
    io:format("~p~n", [Answer]).