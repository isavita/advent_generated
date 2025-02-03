
defmodule Solution do
  def solve do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)

    {wires, gates} =
      Enum.reduce_while(lines, {%{}, []}, fn line, {wires, gates} ->
        cond do
          line == "" -> {:halt, {wires, gates}}
          String.match?(line, ~r/^\w+:\s*[01]$/) ->
            [wire_name, wire_value] = String.split(line, ~r/:?\s+/, trim: true)
            {:cont, {Map.put(wires, wire_name, String.to_integer(wire_value)), gates}}

          String.match?(line, ~r/^\w+\s+(AND|OR|XOR)\s+\w+\s+->\s+\w+$/) ->
            [input1, operation, input2, _, output] = String.split(line, ~r/\s+/, trim: true)
            {:cont, {wires, [%{input1: input1, input2: input2, operation: operation, output: output} | gates]}}

          true ->
            IO.puts("Invalid line: #{line}")
            {:halt, {wires, gates}}
        end
      end)
    
      gates = Enum.reverse(gates)

    wires = simulate(wires, gates, gates)

    z_wires =
      wires
      |> Enum.filter(fn {wire, _} -> String.starts_with?(wire, "z") end)
      |> Enum.map(fn {wire, value} ->
        {String.replace_leading(wire, "z", "") |> String.to_integer(), value}
      end)
      |> Enum.sort_by(fn {index, _} -> index end)

    binary_string =
      z_wires
      |> Enum.map(fn {_, value} -> Integer.to_string(value) end)
      |> Enum.reverse()
      |> Enum.join("")

    binary_string
    |> String.to_integer(2)
    |> IO.puts()
  end

  defp simulate(wires, [], _original_gates), do: wires

  defp simulate(wires, gates, original_gates) do
    progress =
      Enum.reduce_while(gates, {false, [], wires}, fn gate, {progress, new_gates, wires} ->
        case {Map.get(wires, gate.input1), Map.get(wires, gate.input2)} do
          {nil, _} -> {:cont, {progress, [gate | new_gates], wires}}
          {_, nil} -> {:cont, {progress, [gate | new_gates], wires}}
          {val1, val2} ->
            output_val =
              case gate.operation do
                "AND" -> if val1 == 1 and val2 == 1, do: 1, else: 0
                "OR" -> if val1 == 1 or val2 == 1, do: 1, else: 0
                "XOR" -> if val1 != val2, do: 1, else: 0
              end

            {:cont, {true, new_gates, Map.put(wires, gate.output, output_val)}}
        end
      end)

    case progress do
      {false, _, _} ->
        IO.puts("Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.")
        wires

      {true, new_gates, wires} ->
        simulate(wires, Enum.reverse(new_gates), original_gates)
    end
  end
end

Solution.solve()
