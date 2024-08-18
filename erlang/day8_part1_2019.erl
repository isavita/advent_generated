-module(task).
-export([call/0]).

call() ->
  {ok, Data} = file:read_file("input.txt"),
  ImageData = binary_to_list(Data),
  Width = 25,
  Height = 6,
  Layers = split_layers(ImageData, Width, Height, []),
  FewestZeroLayer = find_fewest_zero_layer(Layers),
  Answer = count_1s_and_2s(FewestZeroLayer),
  io:format("~p~n", [Answer]).

split_layers([], _Width, _Height, Layers) ->
  lists:reverse(Layers);
split_layers(Data, Width, Height, Layers) ->
  Layer = lists:sublist(Data, Width * Height),
  NewLayers = [Layer | Layers],
  NewData = lists:nthtail(Width * Height, Data),
  split_layers(NewData, Width, Height, NewLayers).

find_fewest_zero_layer(Layers) ->
  MinZeroLayer = lists:foldl(
    fun (Layer, {MinZeroCount, MinZeroLayer}) ->
      ZeroCount = count_zeros(Layer),
      case ZeroCount < MinZeroCount of
        true -> {ZeroCount, Layer};
        false -> {MinZeroCount, MinZeroLayer}
      end
    end,
    {1000000, []},
    Layers
  ),
  element(2, MinZeroLayer).

count_zeros(Layer) ->
  lists:sum([1 || C <- Layer, C == $0]).

count_1s_and_2s(Layer) ->
  Ones = lists:sum([1 || C <- Layer, C == $1]),
  Twos = lists:sum([1 || C <- Layer, C == $2]),
  Ones * Twos.