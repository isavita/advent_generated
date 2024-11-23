
defmodule ImageDecoder do
  def decode_part1(input, width, height) do
    layers = split_into_layers(input, width, height)
    min_zeros_layer = Enum.min_by(layers, fn layer -> count_digit(layer, 0) end)
    count_digit(min_zeros_layer, 1) * count_digit(min_zeros_layer, 2)
  end

  def decode_part2(input, width, height) do
    layers = split_into_layers(input, width, height)
    decoded_image = decode_image(layers, width, height)
    render_image(decoded_image, width, height)
  end

  defp split_into_layers(input, width, height) do
    layer_size = width * height
    Enum.chunk(input, layer_size)
  end

  defp count_digit(layer, digit) do
    Enum.count(layer, fn x -> x == digit end)
  end

  defp decode_image(layers, width, height) do
    Enum.map(0..(width * height - 1), fn i ->
      decode_pixel(layers, i)
    end)
  end

  defp decode_pixel(layers, index) do
    Enum.find(layers, fn layer ->
      pixel = Enum.at(layer, index)
      pixel != 2
    end) |> (fn layer -> Enum.at(layer, index) end).()
  end

  defp render_image(decoded_image, width, height) do
    Enum.chunk(decoded_image, width)
    |> Enum.map(fn row ->
      Enum.map(row, fn pixel ->
        case pixel do
          0 -> " "
          1 -> "#"
          _ -> " " #Should not happen, but handle for safety
        end
      end) |> Enum.join("")
    end)
    |> Enum.join("\n")
  end
end


input_file = "input.txt"

{:ok, input} = File.read(input_file)
input_digits = input |> String.trim() |> String.graphemes() |> Enum.map(&String.to_integer/1)

width = 25
height = 6

part1_result = ImageDecoder.decode_part1(input_digits, width, height)
IO.puts("Part 1: #{part1_result}")

part2_result = ImageDecoder.decode_part2(input_digits, width, height)
IO.puts("Part 2:\n#{part2_result}")

