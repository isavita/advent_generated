
defmodule Solver do
  def solve(buttons, targets) do
    n = length(targets)
    b = length(buttons)
    m = for i <- 0..(n - 1), into: %{} do
      row = for j <- 0..b, into: %{} do
        val = cond do
          j == b -> Enum.at(targets, i) + 0.0
          i in Enum.at(buttons, j, []) -> 1.0
          true -> 0.0
        end
        {j, val}
      end
      {i, row}
    end

    {m, piv_map, rank} = gaussian(m, n, b)
    piv = Enum.map(0..(n - 1), fn i -> Map.get(piv_map, i, -1) end)

    consistent = Enum.all?(safe_range(rank, n - 1), fn i -> abs(m[i][b]) < 1.0e-9 end)

    if not consistent do
      -1
    else
      is_p = for c <- piv, c >= 0, into: %{}, do: {c, true}
      free = for c <- safe_range(0, b - 1), !Map.has_key?(is_p, c), do: c

      max_vals = for i <- 0..(b - 1) do
        targets_affected = Enum.at(buttons, i)
        |> Enum.filter(&(&1 < n))
        |> Enum.map(&(Enum.at(targets, &1)))

        if targets_affected == [], do: 0, else: Enum.min(targets_affected)
      end |> List.to_tuple()

      sorted_free = Enum.sort_by(free, fn i -> elem(max_vals, i) end)
      piv_tuple = List.to_tuple(piv)

      case backtrack(sorted_free, 0, sorted_free, max_vals, m, piv_tuple, rank, b, [], :infinity) do
        :infinity -> -1
        res -> round(res)
      end
    end
  end

  defp gaussian(m, n, b) do
    Enum.reduce(safe_range(0, b - 1), {m, %{}, 0}, fn c, {m_acc, piv_map, r} ->
      if r >= n do
        {m_acc, piv_map, r}
      else
        mr = Enum.reduce(r..(n - 1), r, fn i, best_i ->
          if abs(m_acc[i][c]) > abs(m_acc[best_i][c]), do: i, else: best_i
        end)

        if abs(m_acc[mr][c]) < 1.0e-9 do
          {m_acc, piv_map, r}
        else
          m_acc = swap_rows(m_acc, r, mr)
          s = m_acc[r][c]
          row_r = m_acc[r]
          scaled_row_r = Enum.reduce(safe_range(c, b), row_r, fn k, acc -> Map.put(acc, k, acc[k] / s) end)
          m_acc = Map.put(m_acc, r, scaled_row_r)

          m_acc = Enum.reduce(0..(n - 1), m_acc, fn i, m_inner ->
            if i == r or abs(m_inner[i][c]) < 1.0e-9 do
              m_inner
            else
              f = m_inner[i][c]
              row_r_inner = m_inner[r]
              row_i = m_inner[i]
              new_row_i = Enum.reduce(safe_range(c, b), row_i, fn k, row_i_acc ->
                Map.put(row_i_acc, k, row_i_acc[k] - f * row_r_inner[k])
              end)
              Map.put(m_inner, i, new_row_i)
            end
          end)
          {m_acc, Map.put(piv_map, r, c), r + 1}
        end
      end
    end)
  end

  defp backtrack([], current_sum, sorted_free, max_vals, m, piv_tuple, rank, b, fv_vals, best) do
    res = Map.new(Enum.zip(sorted_free, Enum.reverse(fv_vals)))

    final_res = Enum.reduce_while(safe_downto(rank - 1, 0), {res, 0}, fn i, {acc_res, acc_sum} ->
      c = elem(piv_tuple, i)
      if c < 0 do
        {:cont, {acc_res, acc_sum}}
      else
        v = m[i][b]
        v = Enum.reduce(safe_range(c + 1, b - 1), v, fn k, v_acc ->
          v_acc - m[i][k] * Map.get(acc_res, k, 0)
        end)
        iv = round(v)
        if abs(v - iv) > 1.0e-6 or iv < 0 or iv > elem(max_vals, c) do
          {:halt, nil}
        else
          {:cont, {Map.put(acc_res, c, iv), acc_sum + iv}}
        end
      end
    end)

    case final_res do
      nil -> best
      {_, p_sum} ->
        total_sum = p_sum + Enum.sum(fv_vals)
        if total_sum < best, do: total_sum, else: best
    end
  end

  defp backtrack([f | rest], current_sum, sorted_free, max_vals, m, piv_tuple, rank, b, fv_vals, best) do
    Enum.reduce(0..elem(max_vals, f), best, fn val, current_best ->
      if current_sum + val >= current_best do
        current_best
      else
        backtrack(rest, current_sum + val, sorted_free, max_vals, m, piv_tuple, rank, b, [val | fv_vals], current_best)
      end
    end)
  end

  defp swap_rows(m, r1, r2) do
    if r1 == r2, do: m, else: (row1 = m[r1]; row2 = m[r2]; m |> Map.put(r1, row2) |> Map.put(r2, row1))
  end

  defp safe_range(a, b) do
    if a <= b, do: a..b, else: []
  end

  defp safe_downto(a, b) do
    if a >= b, do: a..b, else: []
  end

  def main do
    if File.exists?("input.txt") do
      content = File.read!("input.txt")
      lines = content |> String.split("\n") |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == ""))
      total = Enum.reduce(lines, 0, fn line, acc ->
        buttons = Regex.scan(~r/\(([^)]*)\)/, line)
        |> Enum.map(fn [_, m] ->
          s = String.trim(m)
          if s == "", do: [], else: String.split(s, ",") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1)
        end)

        targets = case Regex.run(~r/\{([^}]*)\}/, line) do
          [_, t_str] -> String.split(t_str, ",") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1)
          _ -> nil
        end

        if targets == nil, do: acc, else: acc + solve(buttons, targets)
      end)
      IO.puts(total)
    end
  end
end

Solver.main()
