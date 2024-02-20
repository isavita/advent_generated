defmodule Day13Part2 do
  def call do
    firewall = read_firewall("input.txt")
    delay = find_delay(firewall, 0)
    IO.puts(delay)
  end

  defp read_firewall(filename) do
    File.stream!(filename)
    |> Enum.map(&parse_layer/1)
    |> Enum.into(%{})
  end

  defp parse_layer(line) do
    [depth, range] = line
                      |> String.trim() # Ensure we trim the line to remove newline characters
                      |> String.split(": ") 
                      |> Enum.map(fn x -> x |> String.trim() |> String.to_integer() end)
    {depth, range}
  end

  defp find_delay(firewall, delay) do
    if Enum.all?(firewall, fn {depth, range} ->
        rem(depth + delay, (range - 1) * 2) != 0
      end) do
      delay
    else
      find_delay(firewall, delay + 1)
    end
  end
end

Day13Part2.call()
