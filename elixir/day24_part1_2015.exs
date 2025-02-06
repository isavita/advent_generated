
defmodule Day24 do
  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def solve(packages) do
    target_weight = Enum.sum(packages) |> div(3)

    packages
    |> find_groups(target_weight)
    |> Enum.min_by(fn group -> {length(group), quantum_entanglement(group)} end)
    |> quantum_entanglement()
  end

  def find_groups(packages, target_weight, min_size \\ 1) do
      packages
      |> combinations(min_size)
      |> Enum.filter(fn group -> Enum.sum(group) == target_weight end)
      |> Enum.filter(fn group ->
           remaining = packages -- group
           can_split?(remaining, target_weight)
         end)
      |> case do
        [] -> find_groups(packages, target_weight, min_size + 1)
        groups -> groups
      end
  end
  

  defp can_split?(remaining, target_weight) do
    find_group(remaining, target_weight) != nil
  end

  defp find_group(packages, target_weight, current \\ [], acc \\ []) do
    cond do
      Enum.sum(current) == target_weight ->
        current

      Enum.sum(current) > target_weight ->
        nil

      packages == [] ->
        nil

      true ->
        [h | t] = packages
        case find_group(t, target_weight, [h | current], acc) do
            nil -> find_group(t, target_weight, current, acc)
            result -> result
        end
    end
  end

  def combinations(list, n) do
    do_combinations(list, n, [])
  end

  defp do_combinations(list, 0, acc), do: [Enum.reverse(acc)]
  defp do_combinations([], _n, _acc), do: []

  defp do_combinations([h | t], n, acc) do
    with_h = do_combinations(t, n - 1, [h | acc])
    without_h = do_combinations(t, n, acc)
    with_h ++ without_h
  end

  def quantum_entanglement(group) do
    Enum.reduce(group, 1, &*/2)
  end
end

packages = Day24.read_input("input.txt")
result = Day24.solve(packages)
IO.puts(result)
