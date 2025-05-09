
defmodule ImageEnhancer do
  def main do
    {:ok, content} = File.read("input.txt")
    [algorithm_line | rest] = String.split(content, "\n", trim: false)
    [_empty_line | image_lines] = rest
    algorithm = String.trim(algorithm_line)
    image = Enum.filter(image_lines, &(&1 != "")) |> Enum.map(&String.trim/1)

    enhanced_image = enhance_image(image, algorithm, 2)

    lit_count = count_lit_pixels(enhanced_image)

    IO.puts(lit_count)
  end

  defp enhance_image(image, algorithm, times) do
    Enum.reduce(0..(times - 1), image, fn i, current_image ->
      flip = (i == 1 and String.at(algorithm, 0) == "#")
      apply_algorithm(current_image, algorithm, flip)
    end)
  end

  defp apply_algorithm(image, algorithm, flip) do
    old_rows = length(image)
    old_cols = if old_rows == 0, do: 0, else: String.length(hd(image))

    new_rows = old_rows + 2
    new_cols = old_cols + 2

    Enum.map(0..(new_rows - 1), fn r ->
      chars = Enum.map(0..(new_cols - 1), fn c ->
        index = calculate_index(r - 1, c - 1, image, flip)
        String.at(algorithm, index)
      end)
      Enum.join(chars)
    end)
  end

  defp calculate_index(r, c, image, flip) do
    rows = length(image)
    cols = if rows == 0, do: 0, else: String.length(hd(image))

    offsets = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 0}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]

    Enum.reduce(offsets, 0, fn {dr, dc}, acc ->
      nr = r + dr
      nc = c + dc

      bit = if nr >= 0 and nr < rows and nc >= 0 and nc < cols do
        row_str = Enum.at(image, nr)
        char = String.at(row_str, nc)
        if char == "#", do: 1, else: 0
      else
        if flip, do: 1, else: 0
      end

      (acc * 2) + bit
    end)
  end

  defp count_lit_pixels(image) do
    image
    |> Enum.flat_map(&String.codepoints/1)
    |> Enum.count(&(&1 == "#"))
  end
end

ImageEnhancer.main()
