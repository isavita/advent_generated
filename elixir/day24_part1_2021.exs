
defmodule Solution do
  def read_all(path) do
    File.read!(path)
  end

  def num(w) do
    Enum.reduce(w, 0, fn i, acc -> acc * 10 + i end)
  end

  def main do
    lines = read_all("input.txt") |> String.split("\n")
    {l, k, m} =
      Enum.with_index(lines)
      |> Enum.reduce({[], [], []}, fn {line, i}, {l_acc, k_acc, m_acc} ->
        v =
          case rem(i, 18) do
            4 -> String.split(line) |> List.last() |> String.to_integer()
            5 -> String.split(line) |> List.last() |> String.to_integer()
            15 -> String.split(line) |> List.last() |> String.to_integer()
            _ -> nil
          end

        case rem(i, 18) do
          4 -> {[v | l_acc], k_acc, m_acc}
          5 -> {l_acc, [v | k_acc], m_acc}
          15 -> {l_acc, k_acc, [v | m_acc]}
          _ -> {l_acc, k_acc, m_acc}
        end
      end)
      |> then(fn {l, k, m} -> {Enum.reverse(l), Enum.reverse(k), Enum.reverse(m)} end)

    constraints =
      Enum.reduce(0..13, {[], %{}}, fn i, {stack, constraints_acc} ->
        case Enum.at(l, i) do
          1 -> {[i | stack], constraints_acc}
          26 ->
            {pop, new_stack} = List.pop_at(stack, 0)
            new_constraints = Map.put(constraints_acc, pop, {i, Enum.at(m, pop) + Enum.at(k, i)})
            {new_stack, new_constraints}
          _ -> {stack, constraints_acc}
        end
      end)
      |> then(fn {_, constraints} -> constraints end)

    max_vals =
      Enum.map(0..13, fn i ->
        if Map.has_key?(constraints, i) do
          {j, sum} = Map.get(constraints, i)
          vmax =
            Enum.find(9..1, fn v ->
              v + sum <= 9
            end)

          {i, vmax, j, sum}
        else
          nil
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.reduce(List.duplicate(0, 14), fn {i, vmax, j, sum}, acc ->
        List.replace_at(acc, i, vmax)
        |> List.replace_at(j, vmax + sum)
      end)

    IO.puts(num(max_vals))
  end
end

Solution.main()
