
defmodule PlantGrowth do
  def run do
    {initial_state, rules} = parse_input("input.txt")
    state = initial_state |> String.graphemes() |> Enum.with_index() |> Enum.filter(fn {c, _} -> c == "#" end) |> Enum.map(&elem(&1, 1)) |> MapSet.new()

    final_state = Enum.reduce(0..19, state, fn _, acc -> next_generation(acc, rules) end)
    sum = Enum.sum(final_state)
    IO.puts(sum)
  end

  defp parse_input(file) do
    {initial_state, rules} =
      File.stream!(file)
      |> Enum.reduce({"", %{}}, fn line, {initial, rules} ->
        cond do
          String.contains?(line, "initial state") -> {String.trim(String.split(line, ": ") |> List.last()), rules}
          String.contains?(line, "=>") -> 
            [pattern, result] = String.split(line, " => ")
            {initial, Map.put(rules, pattern, String.trim(result))}
          true -> {initial, rules}
        end
      end)

    {initial_state, rules}
  end

  defp next_generation(state, rules) do
    min = Enum.min(state) - 2
    max = Enum.max(state) + 2
    new_state = Enum.reduce(min..max, MapSet.new(), fn i, acc ->
      pattern = Enum.map(-2..2, fn j -> if MapSet.member?(state, i + j), do: "#", else: "." end) |> Enum.join("")
      if Map.get(rules, pattern) == "#" do
        MapSet.put(acc, i)
      else
        acc
      end
    end)
    new_state
  end
end

PlantGrowth.run()
