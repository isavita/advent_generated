defmodule TreeParser do
  def main do
    "input.txt"
    |> File.read!()
    |> parse_input()
    |> parse_tree(0)
    |> elem(0)
    |> IO.puts()
  end

  defp parse_input(input) do
    input
    |> String.trim()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  defp parse_tree(data, index) do
    {child_count, meta_count} = {Enum.at(data, index), Enum.at(data, index + 1)}
    {child_values, new_index} = parse_children(data, index + 2, child_count, [])

    value =
      if child_count == 0 do
        Enum.sum(Enum.slice(data, new_index, meta_count))
      else
        Enum.reduce(Enum.slice(data, new_index, meta_count), 0, fn meta, acc ->
          if meta > 0 and meta <= child_count do
            acc + Enum.at(child_values, meta - 1)
          else
            acc
          end
        end)
      end

    {value, new_index + meta_count}
  end

  defp parse_children(data, index, count, acc) when count > 0 do
    {child_value, new_index} = parse_tree(data, index)
    parse_children(data, new_index, count - 1, [child_value | acc])
  end

  defp parse_children(_data, index, 0, acc), do: {Enum.reverse(acc), index}
end

TreeParser.main()