# Elixir translation of the given logic. Reads input.txt and prints the answer.

defmodule Gate do
  defstruct [:a, :op, :b]
end

defmodule GateWithOutput do
  defstruct [:gate, :output]
end

defmodule Solver do
  # Parsing: after a blank line, each line like "a OP b -> output"
  def parse(input) do
    if String.contains?(input, "\n\n") do
      [_before, rest] = String.split(input, "\n\n", parts: 2)

      rest
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_line/1)
      |> Enum.filter(& &1)
    else
      []
    end
  end

  def parse_line(line) do
    case String.split(line) do
      [a, op, b, "->", output] when output != "" ->
        %GateWithOutput{
          gate: %Gate{a: a, op: op, b: b},
          output: output
        }
      _ ->
        nil
    end
  end

  def reverse_lookup_key(a, op, b) do
    if a > b, do: "#{b}_#{op}_#{a}", else: "#{a}_#{op}_#{b}"
  end

  def create_lookups(gates) do
    Enum.reduce(gates, {%{}, %{}}, fn gwe, {lookup, rev} ->
      g = gwe.gate
      lookup2 = Map.put(lookup, gwe.output, g)
      rev_key = reverse_lookup_key(g.a, g.op, g.b)
      rev2 = Map.put(rev, rev_key, gwe.output)
      {lookup2, rev2}
    end)
  end

  def swap_wires(pairs, gates, a, b) do
    new_pairs = pairs ++ [{a, b}]
    new_gates =
      Enum.map(gates, fn gwe ->
        out = case gwe.output do
          ^a -> b
          ^b -> a
          other -> other
        end
        %GateWithOutput{gate: gwe.gate, output: out}
      end)

    {new_pairs, new_gates}
  end

  def find_in_map(map, key) do
    Map.get(map, key, "")
  end

  def pad_num(i) do
    s = Integer.to_string(i)
    if String.length(s) < 2, do: "0" <> s, else: s
  end

  # Main solver
  def solution(gates) do
    # number of z-wires present
    num_z =
      Enum.count(gates, fn gw ->
        gw.output != "" and String.starts_with?( gw.output, "z" )
      end)

    pairs = []
    final_pairs = solve_loop(pairs, gates, num_z)

    parts = Enum.flat_map(final_pairs, fn {a, b} -> [a, b] end)
    sorted = Enum.sort(parts)
    Enum.join(sorted, ",")
  end

  def solve_loop(pairs, gates, num_z) when length(pairs) >= 4 do
    # already enough pairs
    Enum.map(pairs, fn {a, b} -> {a, b} end)
  end

  def solve_loop(pairs, gates, num_z) do
    {new_pairs, new_gates} = one_round(pairs, gates, num_z)
    solve_loop(new_pairs, new_gates, num_z)
  end

  def one_round(pairs, gates, num_z) do
    {lookup, reverse_lookup} = create_lookups(gates)

    carry_in = ""
    {pairs_out, gates_out, _swapped, _carry} = do_loop(0, num_z, pairs, gates, carry_in, lookup, reverse_lookup)
    {pairs_out, gates_out}
  end

  def do_loop(i, num_z, pairs, gates, carry_in, lookup, reverse_lookup) do
    if i >= num_z do
      {pairs, gates, false, carry_in}
    else
      xi = "x" <> pad_num(i)
      yi = "y" <> pad_num(i)
      zi = "z" <> pad_num(i)

      swapped = false
      found_adder = ""
      carry_out = carry_in

      {found_adder, carry_out} =
        if i == 0 do
          {
            find_in_map(reverse_lookup, reverse_lookup_key(xi, "XOR", yi)),
            find_in_map(reverse_lookup, reverse_lookup_key(xi, "AND", yi))
          }
        else
          bit = find_in_map(reverse_lookup, reverse_lookup_key(xi, "XOR", yi))
          if bit != "" and carry_in != "" do
            found = find_in_map(reverse_lookup, reverse_lookup_key(bit, "XOR", carry_in))
            if found != "" do
              c1 = find_in_map(reverse_lookup, reverse_lookup_key(xi, "AND", yi))
              c2 = find_in_map(reverse_lookup, reverse_lookup_key(bit, "AND", carry_in))
              carry_out = if c1 != "" and c2 != "", do: find_in_map(reverse_lookup, reverse_lookup_key(c1, "OR", c2)), else: ""
              {found, carry_out}
            else
              {"", carry_in}
            end
          else
            {"", carry_in}
          end
        end

      {pairs2, gates2, swapped2} =
        if found_adder == "" do
          if carry_in != "" and Map.has_key?(lookup, zi) do
            gate = Map.get(lookup, zi)
            bit = find_in_map(reverse_lookup, reverse_lookup_key(xi, "XOR", yi))
            if bit != "" do
              if find_in_map(reverse_lookup, reverse_lookup_key(gate.a, "XOR", carry_in)) != "" do
                {pairs, gates} = swap_wires(pairs, gates, bit, gate.a)
                {pairs, gates, true}
              else
                if find_in_map(reverse_lookup, reverse_lookup_key(gate.b, "XOR", carry_in)) != "" do
                  {pairs, gates} = swap_wires(pairs, gates, bit, gate.b)
                  {pairs, gates, true}
                else
                  {pairs, gates, false}
                end
              end
            else
              {pairs, gates, false}
            end
          else
            {pairs, gates, false}
          end
        else
          if found_adder != zi do
            {pairs, gates} = swap_wires(pairs, gates, found_adder, zi)
            {pairs, gates, true}
          else
            {pairs, gates, false}
          end
        end

      if swapped2 do
        {pairs2, gates2, true, carry_out}
      else
        do_loop(i + 1, num_z, pairs2, gates2, carry_out, lookup, reverse_lookup)
      end
    end
  end

  def to_result_string(pairs) do
    parts = Enum.flat_map(pairs, fn {a, b} -> [a, b] end)
    Enum.sort(parts) |> Enum.join(",")
  end
end

defmodule Main do
  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        gates = Solver.parse(content)
        if length(gates) == 0 do
          :ok
        else
          IO.puts(Solver.solution(gates))
        end
      {:error, _} -> :error
    end
  end
end

Main.main()