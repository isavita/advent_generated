-module(task).
-export([call/0]).

call() ->
  {ok, Data} = file:read_file("input.txt"),
  Lines = string:tokens(binary_to_list(Data), "
"),
  TotalDiff = lists:sum([calculate_encoded_length(Line) - length(Line) || Line <- Lines]),
  io:format("~p~n", [TotalDiff]).

calculate_encoded_length(S) ->
  Encoded = "\"",
  EncodedLength = lists:sum([encoded_char_length(Char) || Char <- S]),
  EncodedLength + 2.

encoded_char_length($\\) -> 2;
encoded_char_length($\") -> 2;
encoded_char_length(_) -> 1.