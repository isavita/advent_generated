
defmodule Solver do
  def main do
    {k, l, m} = parse_input("input.txt")
    constraints = build_constraints(k, l, m)
    min = calculate_min(constraints, k, m)
    IO.puts(num(min))
  end

  defp parse_input(file) do
    {k, l, m} = {[], [], []}
    File.read!(file)
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.reduce({k, l, m}, fn {line, i}, {k, l, m} ->
      case rem(i, 18) do
        4 -> {k, l ++ [parse_div(line)], m}
        5 -> {k ++ [parse_add(line)], l, m}
        15 -> {k, l, m ++ [parse_add(line)]}
        _ -> {k, l, m}
      end
    end)
  end

  defp parse_div(line), do: String.split(line) |> List.last() |> String.to_integer()
  defp parse_add(line), do: String.split(line) |> List.last() |> String.to_integer()

  defp build_constraints(k, l, m) do
    constraints = %{}
    stack = []
    
    Enum.reduce(0..(length(l) - 1), {constraints, stack}, fn i, {constraints, stack} ->
      case Enum.at(l, i) do
        1 -> {constraints, [i | stack]}
        26 -> 
          [pop | stack] = stack
          {Map.put(constraints, pop, {i, Enum.at(m, pop) + Enum.at(k, i)}), stack}
      end
    end)
    |> elem(0)
  end

  defp calculate_min(constraints, k, m) do
    min = List.duplicate(0, 14)

    Enum.reduce(0..13, min, fn i, acc ->
      case Map.get(constraints, i) do
        nil -> acc
        {j, offset} ->
          vmin = max(1, 1 - offset)
          acc = List.replace_at(acc, i, vmin)
          List.replace_at(acc, j, vmin + offset)
      end
    end)
  end

  defp num(w) do
    Enum.reduce(w, 0, fn x, acc -> acc * 10 + x end)
  end
end

Solver.main()
