-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    OrbitMap = lists:foldl(fun(Line, Map) ->
                                [Center, Orbiter] = string:tokens(Line, ")"),
                                case maps:is_key(Center, Map) of
                                    true -> NewMap = maps:update(Center, [Orbiter | maps:get(Center, Map)], Map);
                                    false -> NewMap = maps:put(Center, [Orbiter], Map)
                                end,
                                NewMap
                            end, #{}, Lines),
    TotalOrbits = count_orbits(OrbitMap, "COM", 0),
    io:format("~p~n", [TotalOrbits]).

count_orbits(OrbitMap, Start, Depth) ->
    Orbits = maps:get(Start, OrbitMap, []),
    case length(Orbits) of
        0 -> Depth;
        _ -> Depth + lists:sum([count_orbits(OrbitMap, Orbit, Depth + 1) || Orbit <- Orbits])
    end.