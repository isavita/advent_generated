
defmodule Solver do
  def main do
    data = File.read!("input.txt") |> String.trim() |> String.to_charlist() |> Enum.map(&(&1 - ?0))

    {files, ss, _} = Enum.with_index(data) |> Enum.reduce({[], [], 0}, fn {len, i}, {fs, ss, pos} ->
      if rem(i, 2) == 0 do
        {[%{id: div(i, 2), pos: pos, size: len} | fs], ss, pos + len}
      else
        {fs, (if len > 0, do: [%{pos: pos, size: len} | ss], else: ss), pos + len}
      end
    end)

    {moved, _} = Enum.reduce(files, {[], Enum.reverse(ss)}, fn f, {acc_f, acc_s} ->
      case move(acc_s, f, []) do
        {:ok, new_p, new_ss} -> {[%{f | pos: new_p} | acc_f], new_ss}
        _ -> {[f | acc_f], acc_s}
      end
    end)

    moved
    |> Enum.reduce(0, fn f, acc ->
      acc + div(f.id * f.size * (2 * f.pos + f.size - 1), 2)
    end)
    |> IO.puts()
  end

  defp move([s | t], f, c) when s.pos < f.pos do
    if s.size >= f.size do
      ns = if s.size == f.size, do: t, else: [%{pos: s.pos + f.size, size: s.size - f.size} | t]
      {:ok, s.pos, :lists.reverse(c, ns)}
    else
      move(t, f, [s | c])
    end
  end
  defp move(_, _, _), do: :error
end

Solver.main()

