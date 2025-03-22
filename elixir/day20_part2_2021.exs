
defmodule TrenchMap do
  @default_pixel "."

  def parse_input(filename) do
    {algorithm, image} =
      File.read!(filename)
      |> String.split("\n\n")
      |> List.to_tuple()

    algorithm = String.replace(algorithm, "\n", "") |> String.graphemes()
    image = String.split(image, "\n") |> Enum.map(&String.graphemes/1)

    {algorithm, image}
  end

  def enhance_image(image, algorithm, default_pixel) do
    min_x = 0
    max_x = List.first(image) |> length()
    min_y = 0
    max_y = image |> length()

    new_image =
      for y <- min_y - 1..max_y, x <- min_x - 1..max_x, reduce: [] do
        acc ->
          binary_string =
            for dy <- -1..1, dx <- -1..1, reduce: "" do
              acc2 ->
                new_x = x + dx
                new_y = y + dy

                pixel =
                  if new_x >= min_x and new_x < max_x and new_y >= min_y and new_y < max_y do
                    Enum.at(Enum.at(image, new_y), new_x)
                  else
                    default_pixel
                  end

                acc2 <> (if pixel == "#", do: "1", else: "0")
            end

          index = String.to_integer(binary_string, 2)
          new_pixel = Enum.at(algorithm, index)
          [new_pixel | acc]
      end
      |> Enum.reverse()
      |> Enum.chunk_every(max_x - min_x + 2)

    new_image
  end

  def count_lit_pixels(image) do
    image
    |> List.flatten()
    |> Enum.count(fn pixel -> pixel == "#" end)
  end

  def solve(filename, num_iterations) do
    {algorithm, image} = parse_input(filename)
    default_pixel = @default_pixel

    {final_image, _final_default_pixel} =
      Enum.reduce(1..num_iterations, {image, default_pixel}, fn _, {current_image, current_default_pixel} ->
        new_image = enhance_image(current_image, algorithm, current_default_pixel)
        new_default_pixel = if current_default_pixel == ".", do: Enum.at(algorithm, 0), else: Enum.at(algorithm, 511)
        {new_image, new_default_pixel}
      end)

    count_lit_pixels(final_image)
  end

  def main(args) do
    filename = List.first(args) || "input.txt"

    part1_result = solve(filename, 2)
    IO.puts("Part 1: #{part1_result}")

    part2_result = solve(filename, 50)
    IO.puts("Part 2: #{part2_result}")
  end
end

TrenchMap.main(System.argv())
