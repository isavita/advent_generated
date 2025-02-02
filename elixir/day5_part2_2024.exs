
defmodule Solution do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n", trim: true))
    |> case do
      [rules_lines, updates_lines] ->
        rules =
          rules_lines
          |> Enum.filter(fn line -> line != "" end)
          |> Enum.map(fn line ->
            line
            |> String.split("|", trim: true)
            |> Enum.map(&String.to_integer/1)
          end)

        updates =
          updates_lines
          |> Enum.filter(fn line -> line != "" end)
          |> Enum.map(fn line ->
            line
            |> String.split(",", trim: true)
            |> Enum.map(&String.to_integer/1)
          end)

        {rules, updates}

      _ ->
        {[], []}
    end
  end

  def is_correctly_ordered(update, rules) do
    position = Enum.with_index(update) |> Enum.into(%{}, fn {page, index} -> {page, index} end)

    Enum.all?(rules, fn [x, y] ->
      case {Map.get(position, x), Map.get(position, y)} do
        {x_pos, y_pos} when is_integer(x_pos) and is_integer(y_pos) -> x_pos < y_pos
        _ -> true
      end
    end)
  end

  def sort_update(update, rules) do
    adjacency =
      update
      |> Enum.reduce(%{}, fn page, acc -> Map.put(acc, page, []) end)

    adjacency =
      rules
      |> Enum.reduce(adjacency, fn [x, y], acc ->
        if Map.has_key?(acc, x) and Map.has_key?(acc, y) do
          Map.update!(acc, x, fn list -> [y | list] end)
        else
          acc
        end
      end)

    {sorted, _} =
      update
      |> Enum.reduce({[], %{}}, fn node, {sorted, visited} ->
        topo_sort(node, adjacency, visited, sorted)
      end)

    {:ok, Enum.reverse(sorted)}
  end

  defp topo_sort(node, adjacency, visited, sorted) do
    case Map.get(visited, node) do
      :temp_marked -> throw {:error, "Cycle detected"}
      true -> {sorted, visited}
      _ ->
        visited = Map.put(visited, node, :temp_marked)

        {sorted, visited} =
          adjacency[node]
          |> Enum.reduce({sorted, visited}, fn neighbor, {sorted, visited} ->
             topo_sort(neighbor, adjacency, visited, sorted)
          end)
        visited = Map.put(visited, node, true)

        {[node | sorted], visited}
    end
  end

  def solve(filename) do
    {rules, updates} = read_input(filename)

    updates
    |> Enum.reduce(0, fn update, sum ->
      if !is_correctly_ordered(update, rules) do
        case sort_update(update, rules) do
          {:ok, sorted_update} ->
            middle_page = Enum.at(sorted_update, div(length(sorted_update), 2))
            sum + middle_page

          {:error, _} ->
            sum
        end
      else
        sum
      end
    end)
  end
end

filename = "input.txt"
result = Solution.solve(filename)
IO.puts(result)
