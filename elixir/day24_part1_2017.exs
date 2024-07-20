
defmodule Bridge do
  def main do
    components = File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_component/1)

    max_strength = find_strongest_bridge(components, 0, 0, [])
    IO.puts(max_strength)
  end

  defp parse_component(line) do
    [a, b] = String.split(line, "/") |> Enum.map(&String.to_integer/1)
    {a, b}
  end

  defp find_strongest_bridge(components, port, strength, used) do
    new_strengths = 
      Enum.with_index(components)
      |> Enum.filter(fn {{a, b}, i} -> 
        !Enum.member?(used, i) and (a == port or b == port) 
      end)
      |> Enum.map(fn {{a, b}, i} -> 
        next_port = if a == port, do: b, else: a
        find_strongest_bridge(components, next_port, strength + a + b, [i | used])
      end)

    if new_strengths == [], do: strength, else: Enum.max([strength | new_strengths])
  end
end

Bridge.main()
