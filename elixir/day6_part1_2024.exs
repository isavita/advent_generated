
defmodule Solution do
  def solve() do
    grid =
      "input.txt"
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Stream.map(&String.to_charlist/1)
      |> Enum.to_list()

    {h, w} = {length(grid), length(hd(grid))}
    dirs = [{0, -1}, {1, 0}, {0, 1}, {-1, 0}]

    {{x, y}, {dir_x, dir_y}, dir_idx} =
      Enum.reduce_while(0..(h - 1), nil, fn i, _ ->
        case Enum.reduce_while(0..(w - 1), nil, fn j, _ ->
               case Enum.at(Enum.at(grid, i), j) do
                 ?^ -> {:halt, {{j, i}, Enum.at(dirs, 0), 0}}
                 ?> -> {:halt, {{j, i}, Enum.at(dirs, 1), 1}}
                 ?v -> {:halt, {{j, i}, Enum.at(dirs, 2), 2}}
                 ?< -> {:halt, {{j, i}, Enum.at(dirs, 3), 3}}
                 _ -> {:cont, nil}
               end
             end) do
          nil -> {:cont, nil}
          result -> {:halt, result}
        end
      end)

    visited = MapSet.new([{{x, y}}])

    do_solve(grid, {x, y}, {dir_x, dir_y}, dir_idx, visited, {h, w}, dirs)
    |> MapSet.size()
    |> IO.puts()
  end

  defp do_solve(grid, {x, y}, {dir_x, dir_y}, dir_idx, visited, {h, w}, dirs) do
    nx = x + dir_x
    ny = y + dir_y

    cond do
      nx < 0 or nx >= w or ny < 0 or ny >= h ->
        visited

      Enum.at(Enum.at(grid, ny), nx) == ?# ->
        new_dir_idx = rem(dir_idx + 1, 4)
        {new_dir_x, new_dir_y} = Enum.at(dirs, new_dir_idx)
        do_solve(grid, {x, y}, {new_dir_x, new_dir_y}, new_dir_idx, visited, {h, w}, dirs)

      true ->
        do_solve(
          grid,
          {nx, ny},
          {dir_x, dir_y},
          dir_idx,
          MapSet.put(visited, {nx, ny}),
          {h, w},
          dirs
        )
    end
  end
end

Solution.solve()
