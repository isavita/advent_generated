
defmodule Solution do
  def main do
    {:ok, contents} = File.read("input.txt")
    {rules, updates} = parse_input(contents)
    sum =
      Enum.reduce(updates, 0, fn update, acc ->
        if correctly_ordered?(update, rules) do
          acc + Enum.at(update, div(length(update), 2))
        else
          acc
        end
      end)
    IO.puts(sum)
  end

  defp parse_input(contents) do
    [rules_str, updates_str] = String.split(contents, "\n\n", parts: 2)

    rules =
      rules_str
      |> String.split("\n", trim: true)
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(fn line ->
        line
        |> String.split("|", trim: true)
        |> Enum.map(&String.to_integer/1)
      end)

    updates =
      updates_str
      |> String.split("\n", trim: true)
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(fn line ->
        line
        |> String.split(",", trim: true)
        |> Enum.map(&String.to_integer/1)
      end)

    {rules, updates}
  end

  defp correctly_ordered?(update, rules) do
    position = Enum.with_index(update) |> Enum.into(%{}, fn {page, idx} -> {page, idx} end)
    Enum.all?(rules, fn [x, y] ->
      case {Map.get(position, x), Map.get(position, y)} do
        {nil, _} -> true
        {_, nil} -> true
        {pos_x, pos_y} -> pos_x < pos_y
      end
    end)
  end
end

Solution.main()
