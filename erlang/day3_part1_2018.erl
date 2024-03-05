-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Claims = parse_claims(binary:split(Input, <<"\n">>, [global, trim])),
    Fabric = apply_claims(Claims, maps:new()),
    Answer = count_overlaps(Fabric),
    io:format("~p~n", [Answer]).

parse_claims(Lines) ->
    lists:map(fun parse_claim/1, Lines).

parse_claim(Line) ->
    [<<"#", Id/binary>>, Rest] = binary:split(Line, <<" @ ">>),
    [Coords, Dims] = binary:split(Rest, <<": ">>),
    [X, Y] = binary:split(Coords, <<",">>),
    [W, H] = binary:split(Dims, <<"x">>),
    #{id => binary_to_integer(Id),
      x => binary_to_integer(X),
      y => binary_to_integer(Y),
      w => binary_to_integer(W),
      h => binary_to_integer(H)}.

apply_claims([], Fabric) ->
    Fabric;
apply_claims([Claim | Claims], Fabric) ->
    apply_claims(Claims, apply_claim(Claim, Fabric)).

apply_claim(#{x := X, y := Y, w := W, h := H}, Fabric) ->
    lists:foldl(fun(Px, F) ->
                    lists:foldl(fun(Py, Acc) ->
                                    maps:update_with({Px, Py}, fun(V) -> V + 1 end, 1, Acc)
                                end, F, lists:seq(Y, Y + H - 1))
                end, Fabric, lists:seq(X, X + W - 1)).

count_overlaps(Fabric) ->
    maps:fold(fun(_, V, Acc) when V > 1 -> Acc + 1; (_, _, Acc) -> Acc end, 0, Fabric).
