-module(task).
-export([call/0]).

read_input() ->
    {ok, Binary} = file:read_file("input.txt"),
    Input = binary_to_list(Binary),
    lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Input, " \t\n")).

parse_node([ChildNodes, MetadataCount | Rest]) ->
    {Children, Rest1} = parse_children(ChildNodes, Rest),
    {Metadata, Rest2} = lists:split(MetadataCount, Rest1),
    {{Children, Metadata}, Rest2}.

parse_children(0, Rest) ->
    {[], Rest};
parse_children(N, Rest) ->
    {Child, Rest1} = parse_node(Rest),
    {Children, Rest2} = parse_children(N-1, Rest1),
    {[Child | Children], Rest2}.

sum_metadata({_, Metadata}) ->
    lists:sum(Metadata).

traverse_tree({[], Metadata}) ->
    sum_metadata({[], Metadata});
traverse_tree({Children, Metadata}) ->
    lists:sum(lists:map(fun(C) -> traverse_tree(C) end, Children)) + sum_metadata({Children, Metadata}).

call() ->
    Input = read_input(),
    {Tree, _} = parse_node(Input),
    Answer = traverse_tree(Tree),
    io:format("~p~n", [Answer]).