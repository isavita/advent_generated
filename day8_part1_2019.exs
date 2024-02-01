
defmodule SpaceImageFormat do
  def call do
    input = File.read!("input.txt") |> String.trim()
    layers = String.split(input, "", trim: true) |> Enum.chunk_every(25*6)

    layer_with_fewest_zeros = layers
    |> Enum.min_by(&Enum.count(&1, fn x -> x == "0" end))

    ones = Enum.count(layer_with_fewest_zeros, fn x -> x == "1" end)
    twos = Enum.count(layer_with_fewest_zeros, fn x -> x == "2" end)

    ones * twos
  end
end
