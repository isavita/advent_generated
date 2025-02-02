
defmodule Day8 do
  def read_input(filename) do
    filename
    |> File.read!()
    |> String.trim()
    |> String.split(" ")
    |> Enum.map(&String.to_integer/1)
  end

  def parse_node(data, acc \\ 0) do
    case data do
      [num_children, num_metadata | rest] ->
        {children_acc, remaining_data} = parse_children(rest, num_children, 0)
        {metadata, new_remaining} = Enum.split(remaining_data, num_metadata)
        {acc + children_acc + Enum.sum(metadata), new_remaining}

      _ ->
        {acc, data}
    end
  end

  defp parse_children(data, 0, acc), do: {acc, data}

  defp parse_children(data, num_children, acc) do
    {child_acc, remaining_data} = parse_node(data, 0)
    {new_acc, new_remaining} = parse_children(remaining_data, num_children - 1, acc + child_acc)
    {new_acc, new_remaining}
  end

  def solve(filename \\ "input.txt") do
    data = read_input(filename)
    {result, _} = parse_node(data)
    result
  end
end

Day8.solve() |> IO.puts()
