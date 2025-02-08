
defmodule Day17 do
  defstruct width: 0, height: 0, data: %{}

  defp parse_input(input) do
    lines = String.split(input, "\n", trim: true)
    height = length(lines)
    width = length(String.to_charlist(hd(lines)))

    data =
      for {line, y} <- Enum.with_index(lines),
          {char, x} <- Enum.with_index(String.to_charlist(line)),
          into: %{} do
        {{x, y}, char - ?0}
      end

    %Day17{width: width, height: height, data: data}
  end

  defp in_bounds?(grid, {x, y}),
    do: x >= 0 and x < grid.width and y >= 0 and y < grid.height

  defp neighbors(grid, {x, y}) do
    [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
    |> Enum.map(fn {dx, dy} -> {x + dx, y + dy} end)
    |> Enum.filter(&in_bounds?(grid, &1))
  end

  defp heuristic({x1, y1}, {x2, y2}), do: abs(x1 - x2) + abs(y1 - y2)

  defp opposite({x, y}), do: {-x, -y}

  def astar_constrained(grid, start, goal, min_straight, max_straight) do
    queue = :gb_trees.enter({0, start, {0, 0}, 0}, 0, :gb_trees.empty())
    cost_so_far = %{{start, {0, 0}, 0} => 0}

    do_astar(grid, goal, min_straight, max_straight, queue, cost_so_far)
  end

  defp do_astar(grid, goal, min_straight, max_straight, queue, cost_so_far) do
    case :gb_trees.take_smallest(queue) do
      {{_, current, dir, num_straight}, current_cost, rest_queue} ->
        if current == goal and num_straight >= min_straight do
          current_cost
        else
          neighbors = neighbors(grid, current)

          {next_queue, next_cost_so_far} =
            Enum.reduce(neighbors, {rest_queue, cost_so_far}, fn next,
                                                                {acc_queue, acc_cost_so_far} ->
              new_dir = {elem(next, 0) - elem(current, 0), elem(next, 1) - elem(current, 1)}
              new_num_straight = if new_dir == dir, do: num_straight + 1, else: 1

              new_cost = current_cost + Map.get(grid.data, next)

              cond do
                (num_straight >= min_straight or new_dir == dir or
                   current == {0, 0}) and new_num_straight <= max_straight and
                     new_dir != opposite(dir) ->
                  case Map.get(acc_cost_so_far, {next, new_dir, new_num_straight}) do
                    nil ->
                      priority = new_cost + heuristic(next, goal)

                      updated_queue =
                        :gb_trees.enter(
                          {priority, next, new_dir, new_num_straight},
                          new_cost,
                          acc_queue
                        )

                      updated_cost_so_far =
                        Map.put(acc_cost_so_far, {next, new_dir, new_num_straight}, new_cost)

                      {updated_queue, updated_cost_so_far}

                    existing_cost when new_cost < existing_cost ->
                      priority = new_cost + heuristic(next, goal)

                      updated_queue =
                        :gb_trees.enter(
                          {priority, next, new_dir, new_num_straight},
                          new_cost,
                          acc_queue
                        )

                      updated_cost_so_far =
                        Map.put(acc_cost_so_far, {next, new_dir, new_num_straight}, new_cost)

                      {updated_queue, updated_cost_so_far}

                    _ ->
                      {acc_queue, acc_cost_so_far}
                  end

                true ->
                  {acc_queue, acc_cost_so_far}
              end
            end)

          do_astar(grid, goal, min_straight, max_straight, next_queue, next_cost_so_far)
        end

      :undefined ->
        -1
    end
  end

  def solve(input) do
    grid = parse_input(input)
    start = {0, 0}
    goal = {grid.width - 1, grid.height - 1}
    astar_constrained(grid, start, goal, 4, 10)
  end

  def main() do
    input = File.read!("input.txt")
    IO.puts(solve(input))
  end
end

Day17.main()
