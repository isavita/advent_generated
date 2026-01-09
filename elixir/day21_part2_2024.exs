
defmodule Main do
  @max_robots 26
  @key_pad ["789", "456", "123", " 0A"]
  @robot_pad [" ^A", "<v>"]

  def main do
    :ets.new(:cache, [:named_table, :public, read_concurrency: true])

    content =
      "input.txt"
      |> File.read!()
      |> String.trim()

    total =
      content
      |> String.split("\n", trim: true)
      |> Enum.reduce(0, fn line, acc ->
        numeric =
          line
          |> String.graphemes()
          |> Enum.filter(&(&1 >= "0" and &1 <= "9"))
          |> Enum.reduce(0, fn d, n -> n * 10 + String.to_integer(d) end)

        acc + solve(line, @max_robots, @key_pad, @robot_pad, @max_robots) * numeric
      end)

    IO.puts(total)
  end

  ## CACHED HELPERS ---------------------------------------------------------

  defp cache_get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}
      [] -> :error
    end
  end

  defp cache_put(key, value), do: :ets.insert(:cache, {key, value})

  ## find_position ----------------------------------------------------------

  defp find_position(mat, ch) do
    key = {:find_position, ch, Enum.join(mat, "\n")}
    case cache_get(key) do
      {:ok, v} -> v
      :error ->
        pos =
          mat
          |> Enum.with_index()
          |> Enum.reduce_while({-1, -1}, fn {row, i}, _ ->
            case String.graphemes(row) |> Enum.with_index() |> Enum.find(fn {c, _j} -> c == ch end) do
              {_, j} -> {:halt, {i, j}}
              nil -> {:cont, nil}
            end
          end)

        cache_put(key, pos)
        pos
    end
  end

  ## ok ---------------------------------------------------------------------

  defp ok(mat, {si, sj} = st, seq) do
    key = {:ok, st, seq, Enum.join(mat, "\n")}
    case cache_get(key) do
      {:ok, v} -> v
      :error ->
        {i, j, res} =
          seq
          |> String.graphemes()
          |> Enum.reduce_while({si, sj, true}, fn ch, {i, j, _} ->
            if String.at(Enum.at(mat, i), j) == " " do
              {:halt, {i, j, false}}
            else
              {ni, nj} =
                case ch do
                  "^" -> {i - 1, j}
                  "v" -> {i + 1, j}
                  "<" -> {i, j - 1}
                  ">" -> {i, j + 1}
                end

              cond do
                ni < 0 or ni >= length(mat) or nj < 0 or nj >= String.length(Enum.at(mat, 0)) ->
                  {:halt, {ni, nj, false}}
                true ->
                  {:cont, {ni, nj, true}}
              end
            end
          end)

        cache_put(key, res)
        res
    end
  end

  ## generate_moves ---------------------------------------------------------

  defp generate_moves(pos, obj, pad) do
    key = {:gen_moves, pos, obj, Enum.join(pad, "\n")}
    case cache_get(key) do
      {:ok, v} -> v
      :error ->
        {oi, oj} = find_position(pad, obj)
        {pi, pj} = pos

        moves = 
          (if pj > oj, do: String.duplicate("<", pj - oj), else: "") <>
          (if pi > oi, do: String.duplicate("^", pi - oi), else: "") <>
          (if pi < oi, do: String.duplicate("v", oi - pi), else: "") <>
          (if pj < oj, do: String.duplicate(">", oj - pj), else: "")

        moves = if ok(pad, pos, moves), do: moves, else:
          (if pj < oj, do: String.duplicate(">", oj - pj), else: "") <>
          (if pi > oi, do: String.duplicate("^", pi - oi), else: "") <>
          (if pi < oi, do: String.duplicate("v", oi - pi), else: "") <>
          (if pj > oj, do: String.duplicate("<", pj - oj), else: "")

        cache_put(key, moves)
        moves
    end
  end

  ## solve ------------------------------------------------------------------

  defp solve(code, robots, key_pad, robot_pad, max_robots) do
    key = {:solve, code, robots, max_robots}
    case cache_get(key) do
      {:ok, v} -> v
      :error ->
        result =
          if robots <= 0 do
            String.length(code)
          else
            {start_i, start_j} = {3, 2}
            {si, sj} = if robots != max_robots, do: {0, start_j}, else: {start_i, start_j}

            {ret, _, _} =
              code
              |> String.graphemes()
              |> Enum.reduce({0, si, sj}, fn ch, {acc, pi, pj} ->
                pad = if robots == max_robots, do: key_pad, else: robot_pad
                moves = generate_moves({pi, pj}, ch, pad)
                {ni, nj} = find_position(pad, ch)
                new_acc = acc + solve(moves <> "A", robots - 1, key_pad, robot_pad, max_robots)
                {new_acc, ni, nj}
              end)

            ret
          end

        cache_put(key, result)
        result
    end
  end
end

Main.main()
