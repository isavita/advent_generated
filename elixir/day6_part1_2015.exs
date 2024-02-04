
defmodule FireHazard do
  def call do
    "input.txt"
    |> File.read!()
    |> calculate()
  end

  defp calculate(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{grid: MapSet.new(), width: 1000, height: 1000}, &process_instruction/2)
    |> Map.fetch!(:grid)
    |> MapSet.size()
  end

  defp process_instruction(line, %{grid: grid} = acc) do
    {action, from, to} = parse_line(line)
    Enum.reduce(range(from, to, acc.width), grid, fn {x, y}, acc_grid ->
      case action do
        :toggle -> if MapSet.member?(acc_grid, {x, y}), do: MapSet.delete(acc_grid, {x, y}), else: MapSet.put(acc_grid, {x, y})
        :turn_on -> MapSet.put(acc_grid, {x, y})
        :turn_off -> MapSet.delete(acc_grid, {x, y})
      end
    end)
    |> (fn new_grid -> Map.put(acc, :grid, new_grid) end).()
  end

  defp parse_line(line) do
    [action, from, "through", to] =
      Regex.replace(~r/turn on|turn off|toggle/, line, fn match ->
        case match do
          "turn on" -> "turn_on"
          "turn off" -> "turn_off"
          "toggle" -> "toggle"
        end
      end)
      |> String.split()

    {String.to_atom(action), parse_coords(from), parse_coords(to)}
  end

  defp parse_coords(coords) do
    coords
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  defp range({x1, y1}, {x2, y2}, width) do
    for x <- x1..x2, y <- y1..y2, do: {rem(x, width), rem(y, width)}
  end
end

FireHazard.call() |> IO.puts()
