
defmodule Solution do
  @size 71

  def solve do
    grid =
      File.stream!("input.txt")
      |> Stream.take(1024)
      |> Enum.reduce(List.duplicate(List.duplicate(false, @size), @size), fn line, acc ->
        [x, y] =
          line
          |> String.trim()
          |> String.split(",")
          |> Enum.map(&String.to_integer/1)

        if x >= 0 && x < @size && y >= 0 && y < @size do
          List.replace_at(acc, y, List.replace_at(Enum.at(acc, y), x, true))
        else
          acc
        end
      end)

    dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]
    visited = List.duplicate(List.duplicate(false, @size), @size)

    q = :queue.from_list([{{0, 0}, 0}])
    visited = List.replace_at(visited, 0, List.replace_at(Enum.at(visited, 0), 0, true))

    process_queue(q, grid, visited, dirs)
  end

  defp process_queue(q, grid, visited, dirs) do
    case :queue.out(q) do
      {{:value, {{x, y}, steps}}, q_rest} ->
        if x == @size - 1 && y == @size - 1 do
          IO.puts(steps)
        else
          new_q =
            dirs
            |> Enum.reduce(q_rest, fn {dx, dy}, acc ->
              nx = x + dx
              ny = y + dy

              if nx >= 0 && ny >= 0 && nx < @size && ny < @size &&
                 not Enum.at(Enum.at(grid, ny), nx) &&
                 not Enum.at(Enum.at(visited, ny), nx) do
                new_visited = List.replace_at(visited, ny, List.replace_at(Enum.at(visited, ny), nx, true))

                :queue.in({{nx, ny}, steps + 1}, acc)
              else
                acc
              end
            end)
          
          visited = Enum.reduce(dirs, visited, fn {dx,dy}, acc_v ->
            nx = x+dx
            ny = y+dy
            if nx >= 0 && ny >= 0 && nx < @size && ny < @size &&
                not Enum.at(Enum.at(grid, ny), nx) &&
                not Enum.at(Enum.at(acc_v, ny), nx) do
              List.replace_at(acc_v, ny, List.replace_at(Enum.at(acc_v, ny), nx, true))
            else
              acc_v
            end
          end)

          process_queue(new_q, grid, visited, dirs)
        end

      {:empty, _} ->
        IO.puts("No path")
    end
  end
end

Solution.solve()
