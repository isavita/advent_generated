-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Instructions = binary_to_list(Data),
    {SantaVisits, RoboSantaVisits} = deliver_presents(Instructions, {0, 0}, {0, 0}, [{0, 0}], true),
    TotalVisits = lists:usort(SantaVisits ++ RoboSantaVisits),
    Answer = length(TotalVisits),
    io:format("~p~n", [Answer]).

deliver_presents([], _SantaPos, _RoboSantaPos, Visits, _Turn) ->
    {Visits, Visits};
deliver_presents([H|T], SantaPos, RoboSantaPos, Visits, Turn) ->
    {NewSantaPos, NewRoboSantaPos, NewVisits} = case Turn of
        true -> {move(H, SantaPos), RoboSantaPos, [move(H, SantaPos) | Visits]};
        false -> {SantaPos, move(H, RoboSantaPos), [move(H, RoboSantaPos) | Visits]}
    end,
    deliver_presents(T, NewSantaPos, NewRoboSantaPos, NewVisits, not Turn).

move(Direction, {X, Y}) ->
    case Direction of
        $^ -> {X, Y + 1}; % North
        $v -> {X, Y - 1}; % South
        $> -> {X + 1, Y}; % East
        $< -> {X - 1, Y}  % West
    end.
