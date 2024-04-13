defmodule BridgeBuilder do
  defstruct a: 0, b: 0

  def read_components() do
    "input.txt"
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.split(&1, "/"))
    |> Stream.map(fn [a, b] -> %BridgeBuilder{a: String.to_integer(a), b: String.to_integer(b)} end)
    |> Enum.to_list()
  end

  def find_strongest_longest_bridge(components, used, port, strength, length, state) do
    {max_strength, max_length} = state

    state =
      if length > max_length || (length == max_length && strength > max_strength) do
        {strength, length}
      else
        state
      end

    Enum.with_index(components)
    |> Enum.reduce(state, fn {component, i}, acc ->
      if Enum.at(used, i) || (component.a != port && component.b != port) do
        acc
      else
        next_port = if component.a == port, do: component.b, else: component.a
        new_used = List.replace_at(used, i, true)
        find_strongest_longest_bridge(components, new_used, next_port, strength + component.a + component.b, length + 1, acc)
      end
    end)
  end

  def run() do
    components = read_components()
    used = Enum.map(components, fn _ -> false end)
    {max_strength, _} = find_strongest_longest_bridge(components, used, 0, 0, 0, {0, 0})
    IO.puts(max_strength)
  end
end

BridgeBuilder.run()