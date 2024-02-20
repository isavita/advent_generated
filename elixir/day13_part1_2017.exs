defmodule Day13 do
  def call do
    firewall = read_firewall("input.txt")
    severity = calculate_severity(firewall)
    IO.puts(severity)
  end

  defp read_firewall(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_layer/1)
    |> Enum.into(%{})
  end

  defp parse_layer(line) do
    [depth, range] = String.split(line, ": ", trim: true) |> Enum.map(&String.to_integer/1)
    {depth, range}
  end

  defp calculate_severity(firewall) do
    Enum.reduce(firewall, 0, fn {depth, range}, acc ->
      if caught?(depth, range), do: acc + depth * range, else: acc
    end)
  end

  defp caught?(depth, range) do
    rem(depth, (range - 1) * 2) == 0
  end
end

Day13.call()
