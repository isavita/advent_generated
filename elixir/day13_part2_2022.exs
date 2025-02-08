
defmodule Day13 do
  def compare(left, right) when is_integer(left) and is_integer(right) do
    cond do
      left < right -> :ordered
      left > right -> :unordered
      true -> :continue
    end
  end

  def compare(left, right) when is_list(left) and is_list(right) do
    case {left, right} do
      {[], []} -> :continue
      {[], _} -> :ordered
      {_, []} -> :unordered
      {[h1 | t1], [h2 | t2]} ->
        case compare(h1, h2) do
          :continue -> compare(t1, t2)
          other -> other
        end
    end
  end

  def compare(left, right) when is_integer(left) do
    compare([left], right)
  end

  def compare(left, right) when is_integer(right) do
    compare(left, [right])
  end

  def parse(line) do
    line
    |> String.trim()
    |> Code.eval_string()
    |> elem(0)
  end

  def part1(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn pair ->
      [left, right] =
        pair
        |> String.split("\n", trim: true)
        |> Enum.map(&parse/1)

      compare(left, right)
    end)
    |> Enum.with_index(1)
    |> Enum.filter(fn {result, _} -> result == :ordered end)
    |> Enum.map(fn {_, index} -> index end)
    |> Enum.sum()
  end

  def part2(input) do
    divider1 = [[2]]
    divider2 = [[6]]

    packets =
      input
      |> String.split("\n", trim: true)
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(&parse/1)
      |> Kernel.++([divider1, divider2])
      |> Enum.sort(&compare_for_sort/2)

    index1 = Enum.find_index(packets, &(&1 == divider1)) + 1
    index2 = Enum.find_index(packets, &(&1 == divider2)) + 1

    index1 * index2
  end

  def compare_for_sort(a, b) do
    case compare(a, b) do
      :ordered -> true
      :unordered -> false
      :continue -> false  # Should not occur with sorted input and dividers
    end
  end


  def solve() do
    input = File.read!("input.txt")
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end
end

Day13.solve()
