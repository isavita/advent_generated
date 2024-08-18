-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    S = binary_to_list(Data),
    io:format("~p~n", [first_n_unique(S, 14)]).

first_n_unique(S, N) ->
    lists:foldl(fun(_, {I, _}) when I >= length(S) -> {I, -1};
                  (C, {I, Seen}) ->
                      if
                          I >= N ->
                              Sub = lists:sublist(S, I-N+1, N),
                              Unique = lists:usort(Sub),
                              if
                                  length(Sub) == length(Unique) ->
                                      {I, I};
                                  true ->
                                      {I+1, Seen}
                              end;
                          true -> {I+1, Seen}
                      end
                  end, {N, -1}, S).