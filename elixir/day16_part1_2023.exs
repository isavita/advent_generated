
defmodule Solution do
  @north {0, -1}
  @west {-1, 0}
  @south {0, 1}
  @east {1, 0}

  def solve(input) do
    grid = build_grid(input)
    start = {{0, 0}, @east}

    already_seen = calculate_propagation(grid, start)
    already_energized = calculate_energization(already_seen)

    MapSet.size(already_energized)
  end

  defp build_grid(input) do
    width = input |> List.first() |> String.length()
    height = length(input)

    data =
      for {line, y} <- Enum.with_index(input),
          {char, x} <- Enum.with_index(String.to_charlist(line)),
          char != ?. do
        {{x, y}, char}
      end
      |> Map.new()

    %{width: width, height: height, data: data}
  end

  defp rotate90({x, y}), do: {y, -x}

  defp rotate_neg90({x, y}), do: {-y, x}

  defp is_in_bounds?({{x, y}, _}, %{width: width, height: height}) do
    0 <= x and x < width and 0 <= y and y < height
  end

  defp next_beam(%{data: data} = grid, {origin, dir} = beam) do
    case Map.get(data, origin) do
      nil ->
        [
          {add_coord(origin, dir), dir}
        ]

      ?/ ->
        new_dir =
          if dir == @north or dir == @south,
            do: rotate_neg90(dir),
            else: rotate90(dir)

        [
          {add_coord(origin, new_dir), new_dir}
        ]

      ?\\ ->
        new_dir =
          if dir == @north or dir == @south,
            do: rotate90(dir),
            else: rotate_neg90(dir)

        [
          {add_coord(origin, new_dir), new_dir}
        ]

      ?| when dir == @east or dir == @west ->
        [
          {add_coord(origin, rotate90(dir)), rotate90(dir)},
          {add_coord(origin, rotate_neg90(dir)), rotate_neg90(dir)}
        ]

      ?- when dir == @north or dir == @south ->
        [
          {add_coord(origin, rotate90(dir)), rotate90(dir)},
          {add_coord(origin, rotate_neg90(dir)), rotate_neg90(dir)}
        ]

      _ ->
        [
          {add_coord(origin, dir), dir}
        ]
    end
  end

  defp add_coord({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}

  defp calculate_propagation(grid, start) do
    calculate_propagation_recursive(grid, MapSet.new(), [start])
  end

  defp calculate_propagation_recursive(_grid, already_seen, []), do: already_seen

  defp calculate_propagation_recursive(grid, already_seen, [beam | rest]) do
    if is_in_bounds?(beam, grid) and not MapSet.member?(already_seen, beam) do
      new_already_seen = MapSet.put(already_seen, beam)
      new_beams = next_beam(grid, beam)
      calculate_propagation_recursive(grid, new_already_seen, new_beams ++ rest)
    else
      calculate_propagation_recursive(grid, already_seen, rest)
    end
  end

  defp calculate_energization(already_seen) do
    already_seen
    |> MapSet.to_list()
    |> Enum.map(fn {origin, _} -> origin end)
    |> MapSet.new()
  end
end

input = File.read!("input.txt") |> String.trim() |> String.split("\n")
Solution.solve(input) |> IO.puts()
