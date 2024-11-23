
defmodule CathodeRayTube do
  def run do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> parse_instructions()
    |> execute_instructions()
    |> calculate_signal_strengths()
    |> Enum.sum()
    |> IO.puts()
  end

  defp parse_instructions(lines) do
    Enum.map(lines, fn
      "noop" -> {:noop, 0}
      "addx " <> value -> {:addx, String.to_integer(value)}
    end)
  end

  defp execute_instructions(instructions) do
    Enum.reduce(instructions, {1, 1, []}, fn
      {:noop, _}, {cycle, x, strengths} ->
        {cycle + 1, x, update_strengths(cycle, x, strengths)}

      {:addx, value}, {cycle, x, strengths} ->
        strengths = update_strengths(cycle, x, strengths)
        strengths = update_strengths(cycle + 1, x, strengths)
        {cycle + 2, x + value, strengths}
    end)
    |> elem(2)
  end

  defp update_strengths(cycle, x, strengths) when rem(cycle, 40) == 20 do
    [cycle * x | strengths]
  end

  defp update_strengths(_cycle, _x, strengths), do: strengths

  defp calculate_signal_strengths(strengths), do: Enum.reverse(strengths)
end

CathodeRayTube.run()
