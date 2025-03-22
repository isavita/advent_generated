
defmodule QuantumEntanglement do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def find_ideal_grouping_four(packages) do
    total_weight = Enum.sum(packages)
    group_weight = div(total_weight, 4)

    packages
    |> find_group_with_min_size(group_weight)
    |> Enum.map(&Enum.product/1)
    |> Enum.min()
  end

  def find_group_with_min_size(packages, group_weight) do
    1..length(packages)
    |> Enum.reduce_while([], fn group_size, acc ->
      valid_groups = find_groups(packages, group_size, group_weight)

      if Enum.any?(valid_groups) do
        {:halt, valid_groups}
      else
        {:cont, acc}
      end
    end)
  end

  def find_groups(packages, group_size, group_weight) do
    packages
    |> combinations(group_size)
    |> Enum.filter(fn group -> Enum.sum(group) == group_weight end)
    |> Enum.filter(fn first_group ->
      remaining_packages = packages -- first_group
      can_split_remaining?(remaining_packages, group_weight, 2)
    end)
  end

  def can_split_remaining?(remaining_packages, group_weight, num_groups) do
    cond do
      num_groups == 0 -> true
      length(remaining_packages) < 0 -> false
      true ->
        1..length(remaining_packages)
        |> Enum.any?(fn group_size ->
          remaining_packages
          |> combinations(group_size)
          |> Enum.any?(fn group ->
            if Enum.sum(group) == group_weight do
              remaining = remaining_packages -- group
              can_split_remaining?(remaining, group_weight, num_groups - 1)
            else
              false
            end
          end)
        end)
    end
  end

  def combinations(list, n) do
    combinations(list, n, [])
  end

  defp combinations([], 0, acc) do
    [Enum.reverse(acc)]
  end

  defp combinations(_list, 0, acc) do
    [Enum.reverse(acc)]
  end

  defp combinations([], _n, _acc) do
    []
  end

  defp combinations([head | tail], n, acc) do
    combinations(tail, n - 1, [head | acc]) ++ combinations(tail, n, acc)
  end

  def main do
    "input.txt"
    |> read_input()
    |> find_ideal_grouping_four()
    |> IO.puts()
  end
end

QuantumEntanglement.main()
