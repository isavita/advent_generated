-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Claims = parse_claims(binary:split(Input, <<"\n">>, [global, trim])),
    Fabric = apply_claims(Claims, maps:new()),
    Answer = find_non_overlapping_claim(Claims, Fabric),
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

find_non_overlapping_claim(Claims, Fabric) ->
    lists:foldl(fun(Claim, Acc) ->
                    case is_non_overlapping(Claim, Fabric) of
                        true -> Claim;
                        false -> Acc
                    end
                end, null, Claims).

is_non_overlapping(#{id := _Id, x := X, y := Y, w := W, h := H}, Fabric) ->
    lists:all(fun(Px) ->
                  lists:all(fun(Py) ->
                                maps:get({Px, Py}, Fabric) == 1
                            end, lists:seq(Y, Y + H - 1))
              end, lists:seq(X, X + W - 1)).
