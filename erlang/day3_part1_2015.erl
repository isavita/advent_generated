-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Directions = binary_to_list(File),
    Houses = move_houses([{0,0}], Directions),
    Answer = length(lists:usort(Houses)),
    io:format("~p~n", [Answer]).

move_houses([{X,Y}|T], [Dir|Rest]) ->
    NewPos = case Dir of
                  $^ -> {X,Y+1};
                  $v -> {X,Y-1};
                  $> -> {X+1,Y};
                  $< -> {X-1,Y}
              end,
    move_houses([NewPos|[{X,Y}|T]], Rest);
move_houses([{X,Y}|T], []) ->
    [{X,Y}|T].