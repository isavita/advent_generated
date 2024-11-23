
defmodule LightGrid do
  @grid_size 1000

  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.reduce(Map.new, &process_instruction/2)
    |> Map.values()
    |> Enum.sum()
    |> IO.puts()
  end

  defp process_instruction(instruction, grid) do
    [start_x, start_y, end_x, end_y] = parse_coordinates(instruction)

    for x <- start_x..end_x, y <- start_y..end_y, reduce: grid do
      acc ->
        value = Map.get(acc, {x, y}, 0)
        new_value = update_light(instruction, value)
        Map.put(acc, {x, y}, new_value)
    end
  end

  defp parse_coordinates(instruction) do
    instruction
    |> String.split()
    |> Enum.filter(&Regex.match?(~r/\d+,\d+/, &1))
    |> Enum.flat_map(&String.split(&1, ","))
    |> Enum.map(&String.to_integer/1)
  end

  defp update_light(instruction, value) do
    cond do
      String.starts_with?(instruction, "turn on") -> value + 1
      String.starts_with?(instruction, "turn off") -> max(0, value - 1)
      String.starts_with?(instruction, "toggle") -> value + 2
    end
  end
end

LightGrid.solve()
